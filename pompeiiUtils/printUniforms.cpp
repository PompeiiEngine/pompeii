#include <iostream>

#include <pompeii/pompeii.h>
#include <pompeiiUtils/pompeiiUtils.h>
using namespace pompeii;

int main( )
{
  struct Vertex
  {
    glm::vec3 position;
    glm::vec3 normal;
    glm::vec2 uv;
  };

  std::cout << offsetof( Vertex, position ) << std::endl;
  std::cout << offsetof( Vertex, normal ) << std::endl;
  std::cout << offsetof( Vertex, uv ) << std::endl;

  std::vector<uint32_t> vtx_spv;
  pompeii::utils::GLSLtoSPV( vk::ShaderStageFlagBits::eVertex, R"(#version 450

  layout( location = 0 ) in vec3 inPos;
  layout( location = 1 ) in vec3 inNormal;
  layout( location = 2 ) in vec2 inUV;

  layout( location = 0 ) out vec3 outNormal;
  layout( location = 1 ) out vec3 outColor;

  layout(binding = 0) uniform Ubo0
  {
    mat4 proj;
    mat4 view;
  } ubo0;

  /*layout(binding = 1) uniform Ubo1
  {
    mat4 view;
  };*/

  layout(binding = 2) uniform sampler2D texSampler;

  layout( push_constant ) uniform PushConsts
  {
    mat4 model;
    mat3 normal;
  } pcte;

  void main( ) 
  {
    gl_Position = ubo0.proj * ubo0.view * pcte.model * vec4( inPos.xyz, 1.0 );
    outNormal = pcte.normal * inNormal;
    outColor = texture( texSampler, inUV ).rgb;
  } )", vtx_spv );
  return 0;
}