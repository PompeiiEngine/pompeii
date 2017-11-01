#include "TextureCubemap.h"

#include "Device.h"
#include "PhysicalDevice.h"

#include "utils.hpp"

namespace lava
{
  TextureCubemap::TextureCubemap( const DeviceRef& device_, 
      const std::array< std::string, 6 >& filePaths,
      const std::shared_ptr<CommandPool>& cmdPool,
      const std::shared_ptr<Queue>& queue, vk::Format format,
      vk::ImageUsageFlags imageUsageFlags,
      vk::ImageLayout imageLayout, bool forceLinear )
    : Texture( device_ )
  {
    unsigned int numChannels;

    struct Image
    {
      unsigned char* pixels;
      uint32_t width;
      uint32_t height;
      uint32_t textureChannels;
      uint32_t size;
    };

    std::vector<Image> images;
    uint32_t totalSize = 0;

    images.reserve( filePaths.size( ) );
    for ( uint32_t i = 0; i < filePaths.size( ); ++i )
    {
      unsigned char* pixels = lava::utils::loadImageTexture(
        filePaths[ i ].c_str( ), width, height,
        numChannels );
      if ( !pixels )
      {
        throw;
      }

      // The load function returns the original channel count, 
      // but it was forced to 4 because of the last parameter
      numChannels = 4; // TODO: hardcoded

      uint32_t size = width * height * numChannels
        * sizeof( unsigned char );
      images.push_back( { pixels, width, height,
        numChannels, size } );
      totalSize += size;
    }

    unsigned char* pixels = ( unsigned char* ) malloc( totalSize );
    unsigned char* pixelData = pixels;
    for ( uint32_t i = 0; i < images.size( ); ++i )
    {
      memcpy( pixelData, images[ i ].pixels, images[ i ].size );
      pixelData += ( images[ i ].size / sizeof( unsigned char ) );
    }

    for ( uint32_t i = 0; i < images.size( ); ++i )
    {
      free( images[ i ].pixels );
    }

    vk::FormatProperties formatProps =
      _device->_physicalDevice->getFormatProperties( format );

    // Only use linear tiling if requested (and supported by the device)
    // Support for linear tiling is mostly limited, so prefer to use
    // optimal tiling instead
    // On most implementations linear tiling will only support a very
    // limited amount of formats and features (mip maps, cubemaps, arrays, etc.)
    VkBool32 useStaging = !forceLinear;

    VkDeviceSize texSize = width * height * numChannels;

    vk::Device device = static_cast< vk::Device >( *_device );

    if ( useStaging )
    {
      // Create a host-visible staging buffer that contains the raw image data
      vk::Buffer stagingBuffer;
      vk::DeviceMemory stagingMemory;

      vk::BufferCreateInfo bci;
      bci.size = texSize;
      bci.usage = vk::BufferUsageFlagBits::eTransferSrc;
      bci.sharingMode = vk::SharingMode::eExclusive;

      stagingBuffer = device.createBuffer( bci );
      stagingMemory = _device->allocateBufferMemory( stagingBuffer,
        vk::MemoryPropertyFlagBits::eHostVisible
        | vk::MemoryPropertyFlagBits::eHostCoherent );  // Allocate + bind

                                                        // Copy texture data into staging buffer
      void* data = device.mapMemory( stagingMemory, 0, texSize );
      memcpy( data, pixels, texSize );
      device.unmapMemory( stagingMemory );

      free( pixels );

      // TODO: Generate MipLevels

      // Create Image
      vk::ImageCreateInfo ici;
      ici.imageType = vk::ImageType::e2D;
      ici.format = format;
      ici.mipLevels = 1;  // TODO: Generate MipLevels
      // Cube faces count as array layers in Vulkan
      ici.arrayLayers = 6;
      // This flag is required for cube map images
      ici.flags = vk::ImageCreateFlagBits::eCubeCompatible;
      ici.samples = vk::SampleCountFlagBits::e1;
      ici.tiling = vk::ImageTiling::eOptimal;
      ici.sharingMode = vk::SharingMode::eExclusive;
      ici.initialLayout = vk::ImageLayout::eUndefined;
      ici.extent.width = width;
      ici.extent.height = height;
      ici.extent.depth = 1;
      ici.usage = imageUsageFlags;

      // Ensure that the TRANSFER_DST bit is set for staging
      if ( !( ici.usage & vk::ImageUsageFlagBits::eTransferDst ) )
      {
        ici.usage |= vk::ImageUsageFlagBits::eTransferDst;
      }

      image = device.createImage( ici );
      deviceMemory = _device->allocateImageMemory( image,
        vk::MemoryPropertyFlagBits::eDeviceLocal );  // Allocate + bind

      std::shared_ptr<CommandBuffer> copyCmd = cmdPool->allocateCommandBuffer( );
      copyCmd->beginSimple( vk::CommandBufferUsageFlagBits::eOneTimeSubmit );

      // The sub resource range describes the regions of the image we will be transition
      vk::ImageSubresourceRange subresourceRange;
      // Image only contains color data
      subresourceRange.aspectMask = vk::ImageAspectFlagBits::eColor;
      // Start at first mip level
      subresourceRange.baseMipLevel = 0;
      // We will transition on all mip levels
      subresourceRange.levelCount = 1;// TODO: mipLevels;
                                      // The cubemap texture has six layers
      subresourceRange.layerCount = 6;

      // Image barrier for optimal image (target)
      // Optimal image will be used as destination for the copy
      // Transition image layout VK_IMAGE_LAYOUT_UNDEFINED to VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      utils::setImageLayout(
        copyCmd,
        image,
        vk::ImageLayout::eUndefined,          // Old layout is undefined
        vk::ImageLayout::eTransferDstOptimal, // New layout
        subresourceRange
      );

      // Copy buffer to image
      // todo: we can generate manual mip levels
      vk::BufferImageCopy region = {};
      region.bufferOffset = 0;
      region.bufferRowLength = 0;
      region.bufferImageHeight = 0;
      region.imageSubresource.aspectMask = vk::ImageAspectFlagBits::eColor;
      region.imageSubresource.mipLevel = 0;
      region.imageSubresource.baseArrayLayer = 0;
      region.imageSubresource.layerCount = 1;
      region.imageOffset = vk::Offset3D( 0, 0, 0 );
      region.imageExtent = vk::Extent3D(
        width,
        height,
        1
      );

      static_cast<vk::CommandBuffer>( *copyCmd ).copyBufferToImage(
        stagingBuffer, image,
        vk::ImageLayout::eTransferDstOptimal, { region }
      );

      // Change texture image layout to shader read after all mip levels have been copied
      this->imageLayout = imageLayout;

      // Transition image layout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL to VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      utils::setImageLayout(
        copyCmd,
        image,
        vk::ImageLayout::eTransferDstOptimal, // Older layout
        imageLayout,                          // New layout
        subresourceRange
      );

      // Send command buffer
      copyCmd->end( );

      queue->submitAndWait( copyCmd );

      // Clean up staging resources
      device.destroyBuffer( stagingBuffer );
      _device->freeMemory( stagingMemory );
    }
    else
    {
      // Prefer using optimal tiling, as linear tiling 
      // may support only a small set of features 
      // depending on implementation (e.g. no mip maps, only one layer, etc.)
      assert( formatProps.linearTilingFeatures & vk::FormatFeatureFlagBits::eSampledImage );

      // Check if this support is supported for linear tiling
      vk::Image mappableImage;
      vk::DeviceMemory mappableMemory;

      vk::ImageCreateInfo ici;
      ici.imageType = vk::ImageType::e2D;
      ici.format = format;
      ici.extent.width = width;
      ici.extent.height = height;
      ici.extent.depth = 1;
      ici.mipLevels = 1;
      // This flag is required for cube map images
      ici.flags = vk::ImageCreateFlagBits::eCubeCompatible;
      // Cube faces count as array layers in Vulkan
      ici.arrayLayers = 6;
      ici.samples = vk::SampleCountFlagBits::e1;
      ici.tiling = vk::ImageTiling::eLinear;
      ici.usage = imageUsageFlags;
      ici.sharingMode = vk::SharingMode::eExclusive;
      ici.initialLayout = vk::ImageLayout::eUndefined;

      mappableImage = device.createImage( ici );
      mappableMemory = _device->allocateImageMemory( mappableImage,
        vk::MemoryPropertyFlagBits::eHostVisible | vk::MemoryPropertyFlagBits::eHostCoherent );  // Allocate + bind

      vk::ImageSubresource subRes;
      subRes.aspectMask = vk::ImageAspectFlagBits::eColor;
      subRes.mipLevel = 0;

      // Get sub resources layout 
      // Includes row pitch, size offsets, etc.
      vk::SubresourceLayout subResLayout = device.getImageSubresourceLayout( mappableImage, subRes );

      void* data = device.mapMemory( mappableMemory, 0, texSize );
      memcpy( data, pixels, texSize );
      device.unmapMemory( mappableMemory );

      // Linear tiled images don't need to be staged
      // and can be directly used as textures
      image = mappableImage;
      deviceMemory = mappableMemory;
      imageLayout = imageLayout;

      std::shared_ptr<CommandBuffer> copyCmd = cmdPool->allocateCommandBuffer( );
      copyCmd->beginSimple( vk::CommandBufferUsageFlagBits::eOneTimeSubmit );
      // Setup image memory barrier
      utils::setImageLayout(
        copyCmd,
        image,
        vk::ImageAspectFlagBits::eColor,
        vk::ImageLayout::eUndefined, // Older layout
        imageLayout                  // New layout
      );

      // Send command buffer
      copyCmd->end( );

      queue->submitAndWait( copyCmd );
    }

    // Create default sampler
    vk::SamplerCreateInfo sci;
    sci.setMagFilter( vk::Filter::eLinear );
    sci.setMinFilter( vk::Filter::eLinear );
    sci.setMipmapMode( vk::SamplerMipmapMode::eLinear );
    sci.setAddressModeU( vk::SamplerAddressMode::eClampToEdge );
    sci.setAddressModeV( vk::SamplerAddressMode::eClampToEdge );
    sci.setAddressModeW( vk::SamplerAddressMode::eClampToEdge );
    sci.setMipLodBias( 0.0f );
    sci.setCompareOp( vk::CompareOp::eNever );
    sci.setMinLod( 0.0f );
    sci.setMaxLod( /*useStaging ? mipLevels : 0.0f*/0.0f );
    sci.setMaxAnisotropy( 1.0f );
    sci.setAnisotropyEnable( VK_TRUE );
    sci.setBorderColor( vk::BorderColor::eFloatOpaqueWhite );

    sampler = device.createSampler( sci );


    // Create image view
    vk::ImageViewCreateInfo vci;
    vci.setViewType( vk::ImageViewType::eCube );
    vci.setFormat( format );
    vci.setComponents( {
      vk::ComponentSwizzle::eR,
      vk::ComponentSwizzle::eG,
      vk::ComponentSwizzle::eB,
      vk::ComponentSwizzle::eA
    } );
    // 6 array layers (faces)
    vci.setSubresourceRange( { vk::ImageAspectFlagBits::eColor, 0, 1, 0, 6 } );
    vci.subresourceRange.levelCount = 1;
		// 6 array layers (faces)
    vci.image = image;

    view = device.createImageView( vci );

    updateDescriptor( );
  }
}