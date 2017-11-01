#include <lava/lava.h>
using namespace lava;

#include <routes.h>

struct Vertex
{
  glm::vec4 position;
};

const std::vector<Vertex> vertices =
{
  { { 0.75f,  0.75f, 0.0f, 1.0f } },
  { { -0.75f,  0.75f, 0.0f, 1.0f } },
  { { 0.0f, -0.750f, 0.0f, 1.0f } }
};
const std::vector<uint32_t> indices = { 0, 1, 2 };

class MyApp : public VulkanApp
{
public:
  std::shared_ptr<VertexBuffer> _vertexBuffer;
  std::shared_ptr<IndexBuffer> _indexBuffer;
  std::shared_ptr<lava::engine::Material> material;
  MyApp( char const* title, uint32_t width, uint32_t height )
    : VulkanApp( title, width, height )
  {
    // Vertex buffer
    {
      uint32_t vertexBufferSize = vertices.size( ) * sizeof( Vertex );
      _vertexBuffer = std::make_shared<VertexBuffer>( _device, vertexBufferSize );
      _vertexBuffer->writeData( 0, vertexBufferSize, vertices.data( ) );
    }

    // Index buffer
    {
      uint32_t indexBufferSize = indices.size( ) * sizeof( uint32_t );
      _indexBuffer = std::make_shared<IndexBuffer>( _device, 
        vk::IndexType::eUint32, indices.size( ) );
      _indexBuffer->writeData( 0, indexBufferSize, indices.data( ) );
    }

    material = std::make_shared<lava::engine::BasicTessTriangle>( );
    material->configure( LAVA_EXAMPLES_SPV_ROUTE, _device, _renderPass );
	}
  void doPaint( void ) override
  {
    // create a command pool for command buffer allocation
    std::shared_ptr<CommandPool> commandPool = _device->createCommandPool( vk::CommandPoolCreateFlagBits::eResetCommandBuffer, _queueFamilyIndex );
    std::shared_ptr<CommandBuffer> commandBuffer = commandPool->allocateCommandBuffer( );

    commandBuffer->begin( );

    std::array<float, 4> ccv = { 0.2f, 0.3f, 0.3f, 1.0f };
    commandBuffer->beginRenderPass( _renderPass, 
      _defaultFramebuffer->getFramebuffer( ), vk::Rect2D( { 0, 0 }, 
      _defaultFramebuffer->getExtent( ) ),
      { vk::ClearValue( ccv ), vk::ClearValue( vk::ClearDepthStencilValue( 1.0f, 0 ) ) }, 
      vk::SubpassContents::eInline
    );
    material->bind( commandBuffer ); //commandBuffer->bindGraphicsPipeline( _pipeline );
    _vertexBuffer->bind( commandBuffer ); //commandBuffer->bindVertexBuffer( 0, _vertexBuffer, 0 );
    _indexBuffer->bind( commandBuffer );  //commandBuffer->bindIndexBuffer( _indexBuffer, 0 );
    
    commandBuffer->setViewportScissors( _defaultFramebuffer->getExtent( ) );
    
    commandBuffer->drawIndexed( indices.size( ), 1, 0, 0, 1 );
    commandBuffer->endRenderPass( );

    commandBuffer->end( );

    _graphicsQueue->submit( SubmitInfo{
      { _defaultFramebuffer->getPresentSemaphore( ) },
      { vk::PipelineStageFlagBits::eColorAttachmentOutput },
      commandBuffer,
      _renderComplete
    } );
  }
	void keyEvent(int key, int scancode, int action, int mods)
	{
		switch (key)
		{
		case GLFW_KEY_ESCAPE:
			switch (action)
			{
			case GLFW_PRESS:
				getWindow( )->close( );
				break;
			default:
				break;
			}
			break;
		default:
			break;
		}
	}
};

void glfwErrorCallback(int error, const char* description)
{
	fprintf(stderr, "GLFW Error %d: %s\n", error, description);
}

int main( void )
{
  try
  {
    VulkanApp* app = new MyApp( "Tesselated Triangle", 800, 600 );

    app->getWindow( )->setErrorCallback( glfwErrorCallback );

    while ( app->isRunning( ) )
    {
      app->waitEvents( );
      app->paint( );
    }

    delete app;
  }
  catch ( std::system_error err )
  {
    std::cout << "System Error: " << err.what( ) << std::endl;
  }
  return 0;
}