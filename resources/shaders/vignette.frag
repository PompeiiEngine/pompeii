#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 1) uniform sampler2D texSampler;

layout(early_fragment_tests) in;

layout(location = 0) in vec2 uv;
layout(location = 0) out vec4 fragColor;

float vignette(vec2 uv, float radius, float smoothness)
{
	float diff = radius - distance(uv, vec2(0.5, 0.5));
	return smoothstep(-smoothness, smoothness, diff);
}
float Luminance( in vec4 color )
{
    return (color.r + color.g + color.b ) / 3.0;
}

vec4 Sepia( in vec4 color )
{
    return vec4(
          clamp(color.r * 0.393 + color.g * 0.769 + color.b * 0.189, 0.0, 1.0)
        , clamp(color.r * 0.349 + color.g * 0.686 + color.b * 0.168, 0.0, 1.0)
        , clamp(color.r * 0.272 + color.g * 0.534 + color.b * 0.131, 0.0, 1.0)
        , color.a
    );
}
const float factor = 1.0;
void main( void )
{
	float vignetteValue = vignette(uv, 0.5, 0.5);
  	fragColor = texture(texSampler, uv) * vignetteValue;
  	fragColor = mix(fragColor, Sepia(fragColor), clamp(factor,0.0,1.0) );

  	//fragColor = vec4( uv, 0.0, 1.0 );
}