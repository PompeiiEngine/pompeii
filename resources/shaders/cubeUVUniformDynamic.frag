#version 450
#extension GL_ARB_separate_shader_objects : enable

layout( location = 0 ) in vec2 outUV;
layout( location = 0 ) out vec4 outColor;

void main( )
{
    outColor = vec4( outUV, 0.0, 1.0 );
}