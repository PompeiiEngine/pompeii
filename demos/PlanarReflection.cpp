/**
 * Copyright (c) 2017 - 2018, Lava
 * All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 **/

#include <lava/lava.h>
#include <lavaRenderer/lavaRenderer.h>
using namespace lava;

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include <routes.h>
#include <iomanip>

class CustomRenderer : public VulkanWindowRenderer
{
public:
  CustomRenderer( lava::VulkanWindow *w )
    : VulkanWindowRenderer( )
    , _window( w )
  {
    _window->setWindowTitle( "Planar reflection" );
  }

  struct
  {
    glm::mat4 view;
    glm::mat4 proj;
  } uboVS;

  struct PushConstants
  {
    glm::mat4 model;
    glm::vec3 color;
  } pushConstants;

  struct Vertex
  {
    glm::vec3 pos;
    glm::vec3 color;
    glm::vec2 texCoord;
  };
  const std::vector<Vertex> vertices =
  {
    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { {  0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { {  0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { {  0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { { -0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },

    { { -0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { {  0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { { -0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { { -0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },

    { { -0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { { -0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { { -0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { { -0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },

    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { {  0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { {  0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { {  0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { {  0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },

    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { {  0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { {  0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { {  0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { { -0.5f, -0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { { -0.5f, -0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },

    { { -0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },
    { {  0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 1.0f } },
    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { {  0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 1.0f, 0.0f } },
    { { -0.5f,  0.5f,  0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 0.0f } },
    { { -0.5f,  0.5f, -0.5f },{ 1.0f, 1.0f, 1.0f },{ 0.0f, 1.0f } },

    { { -1.0f, -1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 0.0f, 0.0f } },
    { {  1.0f, -1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 1.0f, 0.0f } },
    { {  1.0f,  1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 1.0f, 1.0f } },
    { {  1.0f,  1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 1.0f, 1.0f } },
    { { -1.0f,  1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 0.0f, 1.0f } },
    { { -1.0f, -1.0f, -0.5f },{ 0.0f, 0.0f, 0.0f },{ 0.0f, 0.0f } }
  };

  void initResources( void ) override
  {
    auto device = _window->device( );

    const std::string pipCacheFile = LAVA_EXAMPLES_ROUTE + 
        std::string( "pipCache.bin" );

    _window->pipelineCache = device->createPipelineCache( pipCacheFile );

    // Vertex buffer
    {
      uint32_t vertexBufferSize = vertices.size( ) * sizeof( Vertex );
      auto stagingBuffer = device->createBuffer( vertexBufferSize,
        vk::BufferUsageFlagBits::eTransferSrc,
        vk::MemoryPropertyFlagBits::eHostVisible | 
        vk::MemoryPropertyFlagBits::eHostCoherent );
      stagingBuffer->writeData( 0, vertexBufferSize, vertices.data( ) );

      vertexBuffer = device->createBuffer( vertexBufferSize,
        vk::BufferUsageFlagBits::eVertexBuffer | 
        vk::BufferUsageFlagBits::eTransferDst,
        vk::MemoryPropertyFlagBits::eDeviceLocal );

      auto cmd = _window->gfxCommandPool( )->allocateCommandBuffer( );
      cmd->begin( );
        stagingBuffer->copy( cmd, vertexBuffer, 0, 0, vertexBufferSize );
      cmd->end( );

      _window->gfxQueue( )->submitAndWait( cmd );
    }

    // MVP buffer
    {
      mvpBuffer = device->createUniformBuffer( sizeof( uboVS ) );
    }

    tex = device->createTexture2D( LAVA_EXAMPLES_IMAGES_ROUTE +
      std::string( "glass.png" ), _window->gfxCommandPool( ), 
      _window->gfxQueue( ), vk::Format::eR8G8B8A8Unorm );

    auto vertexStage = device->createShaderPipelineShaderStage(
      LAVA_EXAMPLES_SPV_ROUTE + std::string( "planar_reflection_vert.spv" ),
      vk::ShaderStageFlagBits::eVertex
    );
    auto fragmentStage = device->createShaderPipelineShaderStage(
      LAVA_EXAMPLES_SPV_ROUTE + std::string( "planar_reflection_frag.spv" ),
      vk::ShaderStageFlagBits::eFragment
    );

    std::vector<DescriptorSetLayoutBinding> dslbs =
    {
      DescriptorSetLayoutBinding( 0, 
        vk::DescriptorType::eUniformBuffer,
        vk::ShaderStageFlagBits::eVertex
      ),
      DescriptorSetLayoutBinding( 1, 
        vk::DescriptorType::eCombinedImageSampler,
        vk::ShaderStageFlagBits::eFragment
      )
    };

    descriptorSetLayout = device->createDescriptorSetLayout( dslbs );

    vk::PushConstantRange pushConstantRange( 
      vk::ShaderStageFlagBits::eVertex,
      0, sizeof( pushConstants )
    );

    pipelineLayout = device->createPipelineLayout( descriptorSetLayout, 
      pushConstantRange );

    PipelineVertexInputStateCreateInfo vertexInput(
      vk::VertexInputBindingDescription( 0, sizeof( Vertex ),
        vk::VertexInputRate::eVertex ),
        {
          vk::VertexInputAttributeDescription( 0, 0, 
            vk::Format::eR32G32B32Sfloat,
            offsetof( Vertex, pos )
          ),
          vk::VertexInputAttributeDescription( 1, 0, 
            vk::Format::eR32G32B32Sfloat,
            offsetof( Vertex, color )
          ),
          vk::VertexInputAttributeDescription( 2, 0, 
            vk::Format::eR32G32Sfloat,
            offsetof( Vertex, texCoord )
          )
        }
    );

    vk::PipelineInputAssemblyStateCreateInfo assembly( {}, 
      vk::PrimitiveTopology::eTriangleList, VK_FALSE );
    PipelineViewportStateCreateInfo viewport( 1, 1 );
    vk::PipelineRasterizationStateCreateInfo rasterization( {}, true,
      false, vk::PolygonMode::eFill, vk::CullModeFlagBits::eNone,
      vk::FrontFace::eCounterClockwise, false, 0.0f, 0.0f, 0.0f, 1.0f );
    PipelineMultisampleStateCreateInfo multisample( vk::SampleCountFlagBits::e1, 
      false, 0.0f, nullptr, false, false );

    vk::StencilOpState stencilOpState( vk::StencilOp::eKeep, 
      vk::StencilOp::eKeep, vk::StencilOp::eKeep, vk::CompareOp::eAlways, 
      0, 0, 0 );
    vk::PipelineDepthStencilStateCreateInfo depthStencilState( {}, true, true, 
      vk::CompareOp::eLessOrEqual, false, false, stencilOpState, stencilOpState,
      0.0f, 0.0f );

    PipelineColorBlendStateCreateInfo colorBlend( false, vk::LogicOp::eNoOp, 
      vk::PipelineColorBlendAttachmentState( 
        false, vk::BlendFactor::eZero, vk::BlendFactor::eZero, 
        vk::BlendOp::eAdd, vk::BlendFactor::eZero, vk::BlendFactor::eZero, 
        vk::BlendOp::eAdd, vk::ColorComponentFlagBits::eR | 
        vk::ColorComponentFlagBits::eG | vk::ColorComponentFlagBits::eB | 
        vk::ColorComponentFlagBits::eA
      ), { 1.0f, 1.0f, 1.0f, 1.0f }
    );
    PipelineDynamicStateCreateInfo dynamic( { 
      vk::DynamicState::eViewport, vk::DynamicState::eScissor,
      vk::DynamicState::eStencilWriteMask
    } );

    pipelines.cube = device->createGraphicsPipeline( _window->pipelineCache,
      // Specify that we will be creating a derivative of this pipeline.
      vk::PipelineCreateFlagBits::eAllowDerivatives,
      { vertexStage, fragmentStage }, vertexInput, assembly, nullptr,
      viewport, rasterization, multisample, depthStencilState, colorBlend, 
      dynamic, pipelineLayout, _window->defaultRenderPass( )
    );

    depthStencilState.stencilTestEnable = VK_TRUE;

    // glStencilFunc( GL_ALWAYS, 1, 0xFF );
    depthStencilState.back.compareOp = vk::CompareOp::eAlways;
    depthStencilState.back.compareMask = 0xff;
    depthStencilState.back.reference = 1;
    // glStencilMask(0xFF);
    //depthStencilState.back.writeMask = 0xff; NOTE: (Dynamic stencil writeMask)
    // glDepthMask(GL_FALSE);
    depthStencilState.depthWriteEnable = VK_FALSE;

    // glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    depthStencilState.back.failOp = vk::StencilOp::eKeep;
    depthStencilState.back.depthFailOp = vk::StencilOp::eKeep;
    depthStencilState.back.passOp = vk::StencilOp::eReplace;

    depthStencilState.front = depthStencilState.back;

    pipelines.plane = device->createGraphicsPipeline( _window->pipelineCache,
      // Modify pipeline info to reflect derivation
      vk::PipelineCreateFlagBits::eDerivative,
      { vertexStage, fragmentStage }, vertexInput, assembly, nullptr,
      viewport, rasterization, multisample, depthStencilState, colorBlend, 
      dynamic, pipelineLayout, _window->defaultRenderPass( ), 0, 
      pipelines.cube, -1
    );

    // glStencilFunc(GL_EQUAL, 1, 0xFF);
    depthStencilState.back.compareOp = vk::CompareOp::eEqual;
    // glStencilMask(0x00);
    //depthStencilState.back.writeMask = 0x00; NOTE: (Dynamic stencil writeMask)
    // glDepthMask( GL_TRUE );
    depthStencilState.depthWriteEnable = VK_TRUE;

    depthStencilState.front = depthStencilState.back;

    pipelines.reflection = device->createGraphicsPipeline( _window->pipelineCache,
      // Modify pipeline info to reflect derivation
      vk::PipelineCreateFlagBits::eDerivative,
      { vertexStage, fragmentStage }, vertexInput, assembly, nullptr,
      viewport, rasterization, multisample, depthStencilState, colorBlend, 
      dynamic, pipelineLayout, _window->defaultRenderPass( ), 0, 
      pipelines.cube, -1
    );

    uboVS.view = glm::lookAt(
      glm::vec3( 2.5f, 2.5f, 2.0f ),
      glm::vec3( 0.0f, 0.0f, 0.0f ),
      glm::vec3( 0.0f, 0.0f, 1.0f )
    );

    std::array<vk::DescriptorPoolSize, 2> poolSize =
    {
      vk::DescriptorPoolSize( vk::DescriptorType::eUniformBuffer, 1 ),
      vk::DescriptorPoolSize( vk::DescriptorType::eCombinedImageSampler, 1 )
    };
    auto dspPool = device->createDescriptorPool( 1, poolSize );

    // Init descriptor set
    descriptorSet = device->allocateDescriptorSet( dspPool, descriptorSetLayout );

    std::vector<WriteDescriptorSet> wdss =
    {
      WriteDescriptorSet( 
        descriptorSet, 0, 0, vk::DescriptorType::eUniformBuffer,
        1, nullptr, DescriptorBufferInfo( mvpBuffer, 0, sizeof( uboVS ) )
      ),
      WriteDescriptorSet(
        descriptorSet, 1, 0, vk::DescriptorType::eCombinedImageSampler, 1, 
        tex->descriptor, nullptr
      )
    };
    device->updateDescriptorSets( wdss, { } );

    _window->pipelineCache->saveToFile( pipCacheFile );
  }

  void nextFrame( void ) override
  {
    if ( Input::isKeyPressed( lava::Keyboard::Key::Esc ) )
    {
      _window->_window->close( );
    }
    
    auto sc = _window->getExtent( );
    uboVS.proj = glm::perspective( glm::radians( 45.0f ), sc.width / 
      ( float ) sc.height, 0.1f, 10.0f );
    uboVS.proj[ 1 ][ 1 ] *= -1;

    mvpBuffer->writeData( 0, sizeof( uboVS ), &uboVS );

    std::array<vk::ClearValue, 2 > clearValues;
    std::array<float, 4> ccv = { 0.2f, 0.3f, 0.3f, 1.0f };
    clearValues[ 0 ].color = vk::ClearColorValue( ccv );
    clearValues[ 1 ].depthStencil = vk::ClearDepthStencilValue( 1.0f, 0 );

    const auto size = _window->swapChainImageSize( );
    auto cmd = _window->currentCommandBuffer( );
    vk::Rect2D rect;
    rect.extent.width = size.x;
    rect.extent.height = size.y;
    cmd->beginRenderPass(
      _window->defaultRenderPass( ),
      _window->currentFramebuffer( ),
      rect, clearValues, vk::SubpassContents::eInline
    );

    cmd->bindDescriptorSets( vk::PipelineBindPoint::eGraphics, 
      pipelineLayout, 0, { descriptorSet }, {} );

    cmd->setViewportScissors( _window->getExtent( ) );

    cmd->bindVertexBuffer( 0, vertexBuffer, 0 );
    
    vk::PipelineLayout pipl = *pipelineLayout;

    static auto startTime = std::chrono::high_resolution_clock::now( );

    auto currentTime = std::chrono::high_resolution_clock::now( );
    float time = std::chrono::duration_cast<std::chrono::milliseconds>( currentTime - startTime ).count( ) / 1000.0f;

    // Draw cube
    {
      pushConstants.model = glm::rotate( glm::mat4( 1.0f ),
        time * 0.5f * glm::radians( 180.0f ), glm::vec3( 0.0f, 0.0f, 1.0f )
      );

      pushConstants.color = glm::vec3( 1.0f, 1.0f, 1.0f );
      cmd->pushConstants<PushConstants>( pipl,
        vk::ShaderStageFlagBits::eVertex, 0, pushConstants
        );

      cmd->bindGraphicsPipeline( pipelines.cube );
      cmd->draw( 36, 1, 0, 0 );
    }

    // Draw floor
    {
      cmd->bindGraphicsPipeline( pipelines.plane );

      cmd->setStencilWriteMask( vk::StencilFaceFlagBits::eVkStencilFrontAndBack, 0xff );

      cmd->draw( 6, 1, 36, 0 );
    }

    // Draw cube reflection
    {
      pushConstants.model = glm::scale(
        glm::translate( pushConstants.model, glm::vec3( 0.0f, 0.0f, -1.0f ) ),
        glm::vec3( 1.0f, 1.0f, -1.0f )
      );
      pushConstants.color = glm::vec3( 0.3f, 0.3f, 0.3f );
      cmd->pushConstants<PushConstants>( pipl,
        vk::ShaderStageFlagBits::eVertex, 0, pushConstants
      );
      cmd->bindGraphicsPipeline( pipelines.reflection );

      cmd->setStencilWriteMask( vk::StencilFaceFlagBits::eVkStencilFrontAndBack, 0x00 );

      cmd->draw( 36, 1, 0, 0 );
    }

    cmd->endRenderPass( );

    _window->requestUpdate( );
  }

private:
  VulkanWindow *_window;
  std::shared_ptr< Texture2D > tex;
  std::shared_ptr< DescriptorSetLayout > descriptorSetLayout;
  std::shared_ptr< DescriptorSet > descriptorSet;
  std::shared_ptr< PipelineLayout > pipelineLayout;

  struct
  {
    std::shared_ptr<Pipeline> cube;
    std::shared_ptr<Pipeline> plane;
    std::shared_ptr<Pipeline> reflection;
  } pipelines;

  std::shared_ptr< Buffer > vertexBuffer;
  std::shared_ptr< UniformBuffer > mvpBuffer;
};

class CustomVkWindow : public VulkanWindow
{
public:
  VulkanWindowRenderer* createRenderer( void ) override
  {
    return new CustomRenderer( this );
  }
};

int main( void )
{
  std::shared_ptr<Instance> instance;

  // Create instance
  vk::ApplicationInfo appInfo(
    "App Name",
    VK_MAKE_VERSION( 1, 0, 0 ),
    "FooEngine",
    VK_MAKE_VERSION( 1, 0, 0 ),
    VK_API_VERSION_1_0
  );


  std::vector<const char*> layers =
  {
#ifndef NDEBUG
    "VK_LAYER_LUNARG_standard_validation",
#endif
  };
  std::vector<const char*> extensions =
  {
    VK_KHR_SURFACE_EXTENSION_NAME,  // Surface extension
    LAVA_KHR_EXT, // OS specific surface extension
    VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  };


  instance = Instance::create( vk::InstanceCreateInfo(
    { },
    &appInfo,
    layers.size( ),
    layers.data( ),
    extensions.size( ),
    extensions.data( )
  ) );

  CustomVkWindow w;
  w.setVulkanInstance( instance );
  w.resize( 500, 500 );

  w.show( );

  return 0;
}