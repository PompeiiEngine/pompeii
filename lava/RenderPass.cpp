#include "RenderPass.h"
#include "Device.h"

namespace lava
{
  RenderPass::RenderPass( const DeviceRef& device, 
    vk::ArrayProxy<const vk::AttachmentDescription> attachments, 
    vk::ArrayProxy<const vk::SubpassDescription> subpasses,
    vk::ArrayProxy<const vk::SubpassDependency> dependencies )
    : VulkanResource( device )
  {
    vk::RenderPassCreateInfo rpci;
    rpci.setPAttachments( attachments.data( ) );
    rpci.setAttachmentCount( attachments.size( ) );
    rpci.setPDependencies( dependencies.data( ) );
    rpci.setDependencyCount( dependencies.size( ) );
    rpci.setPSubpasses( subpasses.data( ) );
    rpci.setSubpassCount( subpasses.size( ) );
    _renderPass = static_cast< vk::Device >( *_device ).createRenderPass( rpci );
  }

  RenderPass::~RenderPass( )
  {
    static_cast< vk::Device >( *_device ).destroyRenderPass( _renderPass );
  }
}