#include <iostream>

#include <glfwLava/glfwLava.h>
using namespace lava;

#include <glm/glm.hpp>

#include <routes.h>

class MainWindowRenderer : public lava::GLFWVulkanWindowRenderer
{
private:
  lava::GLFWVulkanWindow* _window;
public:
  MainWindowRenderer( lava::GLFWVulkanWindow* window )
    : _window( window )
  {
  }
  
  struct PushConstant
  {
    float time;
  } pc;

  void initResources( void ) override
  {
    auto device = _window->device( );

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

    // Init descriptor and pipeline layouts
    auto descriptorSetLayout = device->createDescriptorSetLayout( { } );

    pipelineLayout = device->createPipelineLayout( descriptorSetLayout, 
      vk::PushConstantRange( 
        vk::ShaderStageFlagBits::eVertex, 0, sizeof( pc )
      )
    );

    // init pipeline
    auto vertexStage = device->createShaderPipelineShaderStage(
      LAVA_EXAMPLES_SPV_ROUTE + std::string( "geometryFigures_vert.spv" ),
      vk::ShaderStageFlagBits::eVertex
    );
    auto geomStage = device->createShaderPipelineShaderStage(
      LAVA_EXAMPLES_SPV_ROUTE + std::string( "geometryFigures_geom.spv" ),
      vk::ShaderStageFlagBits::eGeometry
    );
    auto fragmentStage = device->createShaderPipelineShaderStage(
      LAVA_EXAMPLES_SPV_ROUTE + std::string( "geometryFigures_frag.spv" ),
      vk::ShaderStageFlagBits::eFragment
    );

    vk::VertexInputBindingDescription binding(
      0, sizeof( Vertex ), vk::VertexInputRate::eVertex
    );

    PipelineVertexInputStateCreateInfo vertexInput( binding,
    {
      vk::VertexInputAttributeDescription( 0, 0,
      vk::Format::eR32G32B32Sfloat, offsetof( Vertex, position )
      ),
      vk::VertexInputAttributeDescription( 1, 0,
        vk::Format::eR32G32B32A32Sfloat, offsetof( Vertex, color )
      ),
      vk::VertexInputAttributeDescription( 2, 0,
        vk::Format::eR16Uint, offsetof( Vertex, sides )
      )
    } );
    vk::PipelineInputAssemblyStateCreateInfo assembly( { }, 
      vk::PrimitiveTopology::ePointList, VK_FALSE
    );
    PipelineViewportStateCreateInfo viewport( 1, 1 );
    vk::PipelineRasterizationStateCreateInfo rasterization( { }, true, false,
      vk::PolygonMode::eFill, vk::CullModeFlagBits::eBack,
      vk::FrontFace::eClockwise, false, 0.0f, 0.0f, 0.0f, 1.0f
    );
    PipelineMultisampleStateCreateInfo multisample( vk::SampleCountFlagBits::e1,
      false, 0.0f, nullptr, false, false
    );
    vk::StencilOpState stencilOpState( vk::StencilOp::eKeep,
      vk::StencilOp::eKeep, vk::StencilOp::eKeep, vk::CompareOp::eAlways,
      0, 0, 0
    );
    vk::PipelineDepthStencilStateCreateInfo depthStencil( { }, true, true,
      vk::CompareOp::eLessOrEqual, false, false, stencilOpState,
      stencilOpState, 0.0f, 0.0f
    );
    vk::PipelineColorBlendAttachmentState colorBlendAttachment( false,
      vk::BlendFactor::eZero, vk::BlendFactor::eZero, vk::BlendOp::eAdd,
      vk::BlendFactor::eZero, vk::BlendFactor::eZero, vk::BlendOp::eAdd,
      vk::ColorComponentFlagBits::eR | vk::ColorComponentFlagBits::eG |
      vk::ColorComponentFlagBits::eB | vk::ColorComponentFlagBits::eA
    );
    PipelineColorBlendStateCreateInfo colorBlend( false, vk::LogicOp::eNoOp,
      colorBlendAttachment, { 1.0f, 1.0f, 1.0f, 1.0f }
    );
    PipelineDynamicStateCreateInfo dynamic( { 
      vk::DynamicState::eViewport, vk::DynamicState::eScissor
    } );

    pipeline = device->createGraphicsPipeline( _window->pipelineCache( ), { }, 
      { vertexStage, geomStage, fragmentStage }, vertexInput, assembly, nullptr, 
      viewport, rasterization, multisample, depthStencil, colorBlend, dynamic, 
      pipelineLayout, _window->renderPass( ) );
  }

  void nextFrame( void ) override
  {
    static auto startTime = std::chrono::high_resolution_clock::now( );

    auto currentTime = std::chrono::high_resolution_clock::now( );
    float time = std::chrono::duration_cast<std::chrono::milliseconds>( 
      currentTime - startTime ).count( ) / 1000.0f;

    pc.time = time;

    std::array<vk::ClearValue, 2 > clearValues;
    std::array<float, 4> ccv = { 0.0f, 0.0f, 0.0f, 1.0f };
    clearValues[ 0 ].color = vk::ClearColorValue( ccv );
    clearValues[ 1 ].depthStencil  = vk::ClearDepthStencilValue(  1.0f, 0 );

    const auto size = _window->swapchainImageSize( );
    auto cmd = _window->currentCommandBuffer( );
    vk::Rect2D rect;
    rect.extent = size;
    cmd->beginRenderPass(
      _window->renderPass( ),
      _window->framebuffer( ),
      rect, clearValues, vk::SubpassContents::eInline
    );
    
    cmd->bindGraphicsPipeline( pipeline );
    cmd->bindVertexBuffer( 0, vertexBuffer, 0 );

    cmd->setViewportScissors( size );
    cmd->pushConstants<PushConstant>( *pipelineLayout, 
      vk::ShaderStageFlagBits::eVertex, 0, pc );
    cmd->draw( vertices.size( ), 1, 0, 0 );
    cmd->endRenderPass( );

    _window->frameReady( );
  }
private:
  struct Vertex
  {
    glm::vec2 position;
    glm::vec3 color;
    uint32_t sides;
  };

  std::vector<Vertex> vertices =
  {
    //   Coordinates             Color        Sides
    { { -0.45f,  0.45f }, { 1.0f, 0.0f, 0.0f },  3 },
    { {   0.0f,   0.0f }, { 1.0f, 1.0f, 1.0f },  4 },
    { {  0.45f,  0.45f }, { 0.0f, 1.0f, 0.0f },  5 },
    { {  0.45f, -0.45f }, { 0.0f, 0.0f, 1.0f },  8 },
    { { -0.45f, -0.45f }, { 1.0f, 1.0f, 0.0f }, 16 }
  };

  std::shared_ptr<Buffer> vertexBuffer;
  std::shared_ptr<Pipeline> pipeline;
  std::shared_ptr<PipelineLayout> pipelineLayout;
};

class VulkanWindow : public lava::GLFWVulkanWindow
{
public:
  explicit VulkanWindow( int width, int height,
    const std::string& title, bool enableLayers )
    : lava::GLFWVulkanWindow( width, height, title, enableLayers )
  {

  }
  /*VulkanWindow( int w, int h, const char* title, bool enableLayers )
    : lava::GLFWVulkanWindow( w, h, title, enableLayers )
  {

  }*/
  virtual lava::GLFWVulkanWindowRenderer* createRenderer( void ) override
  {
    return new MainWindowRenderer( this );
  }
};


int main( int argc, char** argv )
{
  VulkanWindow app( 500, 500, "Geometry Figures", true );
  app.show( );
  return EXIT_SUCCESS;
}