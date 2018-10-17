#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 1) uniform samplerCube skybox;
layout(binding = 2) uniform UniformBufferObject
{
    vec3 viewPos;
} ubo;

layout (location = 0) in vec3 Position;
layout (location = 1) in vec3 Normal;
layout (location = 0) out vec4 outColor;

void main( )
{
    float ratio = 1.00 / 1.52;
    vec3 I = normalize(Position - ubo.viewPos);
    vec3 R = refract(I, normalize(Normal), ratio);
    outColor = vec4(texture(skybox, R).rgb, 1.0);
}