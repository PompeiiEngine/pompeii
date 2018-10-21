/**
 * Copyright (c) 2017 - 2018, Pompeii
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

#include "Compiler.h"
#include <glslang/SPIRV/GlslangToSpv.h>
#include <map>
#include <set>

namespace std
{
  template<typename T, typename... Args>
  std::unique_ptr<T> make_unique(Args&&... args) {
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
  }
}

namespace pompeii
{
  namespace utils
  {
    class Uniform
    {
    public:
      std::string _name;
      int32_t _binding;
      int32_t _offset;
      int32_t _size;
      int32_t _glType;
      vk::ShaderStageFlags _stageFlags;
      Uniform(const std::string &name, const int32_t &binding, 
        const int32_t &offset, const int32_t &size,
        const int32_t &glType, const vk::ShaderStageFlagBits &stageFlags) :
        _name(name),
        _binding(binding),
        _offset(offset),
        _size(size),
        _glType(glType),
        _stageFlags(stageFlags)
      {
      }
      bool operator==(const Uniform &other) const
      {
        return _name == other._name && _binding == other._binding && _offset == other._offset && _glType == other._glType;
      }

      bool operator!=(const Uniform &other) const
      {
        return !(*this == other);
      }

      std::string ToString() const
      {
        std::stringstream result;
        result << "Uniform(name '" << _name << "', binding " 
          << _binding << ", offset " << _offset << ", size "
          << _size << ", glType ";

        // https://github.com/KhronosGroup/glslang/blob/master/glslang/MachineIndependent/gl_types.h
        switch (_glType)
        {
        case 0x8B5E: // GL_SAMPLER_2D
        case 0x904D: // GL_IMAGE_2D
        case 0x9108: // GL_SAMPLER_2D_MULTISAMPLE
        case 0x9055: // GL_IMAGE_2D_MULTISAMPLE
          result << "IMAGE 2D";
          break;
        case 0x8B60: // GL_SAMPLER_CUBE
        case 0x9050: // GL_IMAGE_CUBE
          result << "IMAGE CUBE";
        case 0x8B5F: // GL_SAMPLER_3D
        case 0x904E: // GL_IMAGE_3D
          result << "IMAGE 3D";
          break;

        case 0x8B5B: // GL_FLOAT_MAT3
          result << "MAT3";
          break;
        case 0x8B5C: // GL_FLOAT_MAT4
          result << "MAT4";
          break;
        default:
          result << "none";
          break;
        }


        result << ")";
        return result.str();
      }
    };

    class VertexAttribute
    {
    public:
      std::string _name;
      int32_t _location;
      int32_t _size;
      int32_t _glType;
      VertexAttribute(const std::string &name, const int32_t &location,
        const int32_t &size, const int32_t &glType) :
        _name(name),
        _location(location),
        _size(size),
        _glType(glType)
      {
      }

      std::string ToString() const
      {
        std::stringstream result;
        result << "VertexAttribute(name '" << _name <<
          "', location " << _location <<
          ", size " << _size << ", glType ";

        switch( _glType )
        {
        case 0x1406: // GL_FLOAT
          result << "VK_FORMAT_R32_SFLOAT";
          break;
        case 0x8B50: // GL_FLOAT_VEC2
          result << "VK_FORMAT_R32G32_SFLOAT";
          break;
        case 0x8B51: // GL_FLOAT_VEC3
          result << "VK_FORMAT_R32G32B32_SFLOAT";
          break;
        case 0x8B52: // GL_FLOAT_VEC4
          result << "VK_FORMAT_R32G32B32A32_SFLOAT";
          break;
        case 0x1404: // GL_INT
          result << "VK_FORMAT_R32_SINT";
          break;
        case 0x8B53: // GL_INT_VEC2
          result << "VK_FORMAT_R32G32_SINT";
          break;
        case 0x8B54: // GL_INT_VEC3
          result << "VK_FORMAT_R32G32B32_SINT";
          break;
        case 0x8B55: // GL_INT_VEC4
          result << "VK_FORMAT_R32G32B32A32_SINT";
          break;
        case 0x1405: // GL_UNSIGNED_INT
          result << "VK_FORMAT_R32_SINT";
          break;
        case 0x8DC6: // GL_UNSIGNED_INT_VEC2
          result << "VK_FORMAT_R32G32_SINT";
          break;
        case 0x8DC7: // GL_UNSIGNED_INT_VEC3
          result << "VK_FORMAT_R32G32B32_SINT";
          break;
        case 0x8DC8: // GL_UNSIGNED_INT_VEC4
          result << "VK_FORMAT_R32G32B32A32_SINT";
          break;
        default:
          result << "VK_FORMAT_UNDEFINED";
          break;
        }

        result << ")";
        return result.str();
      }
    };

    enum UniformBlockType
    {
      BLOCK_UNIFORM = 0,
      BLOCK_STORAGE = 1,
      BLOCK_PUSH = 2
    };

    class UniformBlock
    {
    public:
      std::string _name;
      int32_t _binding;
      int32_t _size;
      vk::ShaderStageFlags _stageFlags;
      UniformBlockType _type;
      std::vector<std::unique_ptr<Uniform>> _uniforms;
      UniformBlock(const std::string &name, const int32_t &binding, 
        const int32_t &size, const vk::ShaderStageFlagBits &stageFlags, 
        const UniformBlockType &type) :
        _name(name),
        _binding(binding),
        _size(size),
        _stageFlags(stageFlags),
        _type(type),
        _uniforms(std::vector<std::unique_ptr<Uniform>>())
      {
      }
      UniformBlock(const UniformBlock&) = delete; 

      UniformBlock& operator=(UniformBlock&) = delete;

      void addUniform(Uniform *uniform)
      {
        for (const auto &u : _uniforms)
        {
          if (*u == *uniform)
          {
            return;
          }
        }
        _uniforms.emplace_back(uniform);
      }

      Uniform* getUniform(const std::string &uniformName)
      {
        for (const auto &uniform : _uniforms)
        {
          if (uniform->_name == uniformName)
          {
            return uniform.get();
          }
        }

        return nullptr;
      }

      std::string ToString() const
      {
        std::stringstream result;
        result << "UniformBlock(name '" << _name <<
          "', binding " << _binding << ", size " <<
          _size << ", type ";

        switch (_type)
        {
          case BLOCK_UNIFORM:
            result << "BLOCK_UNIFORM";
            break;
          case BLOCK_STORAGE:
            result << "BLOCK_STORAGE";
            break;
          case BLOCK_PUSH:
            result << "BLOCK_PUSH";
            // Push constants are described in the pipeline.
            break;
          default:
            break;
        }

        if(!_uniforms.empty( ))
        {
          for( const auto& u: _uniforms)
          {
            result << "\n\t\t" << u->ToString( ) << "\n";
          }
          result << "\t)";
        }
        else
        {
          result << ")";
        }

        return result.str();
      }

      friend bool operator< (const Uniform &c1, const Uniform &c2)
      {
        return c1._offset < c2._offset;
      }
    };

    class GLSLToSPIRVCompiler
    {
    public:
      POMPEIIUTILS_API
      GLSLToSPIRVCompiler();
      POMPEIIUTILS_API
      ~GLSLToSPIRVCompiler();

      POMPEIIUTILS_API
      bool compile(vk::ShaderStageFlagBits stage,
      const std::string& source, std::vector<uint32_t> &spirv);


      void ProcessShader( void );

      void LoadProgram( const glslang::TProgram& program, 
          const vk::ShaderStageFlagBits& stage );
      void LoadUniformBlock(const glslang::TProgram &program, 
          const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i);
      void LoadUniform(const glslang::TProgram &program, 
          const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i);
      void LoadVertexAttribute(const glslang::TProgram &program, 
          const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i);


      std::vector<std::unique_ptr<Uniform>> _uniforms;
      std::vector<std::unique_ptr<UniformBlock>> _uniformBlocks;
      std::vector<std::unique_ptr<VertexAttribute>> _vertexAttributes;

    private:
      TBuiltInResource  _resource;
    };

    GLSLToSPIRVCompiler::GLSLToSPIRVCompiler()
    {
    #ifndef __ANDROID__
      glslang::InitializeProcess();
    #endif

      _resource.maxLights = 32;
      _resource.maxClipPlanes = 6;
      _resource.maxTextureUnits = 32;
      _resource.maxTextureCoords = 32;
      _resource.maxVertexAttribs = 64;
      _resource.maxVertexUniformComponents = 4096;
      _resource.maxVaryingFloats = 64;
      _resource.maxVertexTextureImageUnits = 32;
      _resource.maxCombinedTextureImageUnits = 80;
      _resource.maxTextureImageUnits = 32;
      _resource.maxFragmentUniformComponents = 4096;
      _resource.maxDrawBuffers = 32;
      _resource.maxVertexUniformVectors = 128;
      _resource.maxVaryingVectors = 8;
      _resource.maxFragmentUniformVectors = 16;
      _resource.maxVertexOutputVectors = 16;
      _resource.maxFragmentInputVectors = 15;
      _resource.minProgramTexelOffset = -8;
      _resource.maxProgramTexelOffset = 7;
      _resource.maxClipDistances = 8;
      _resource.maxComputeWorkGroupCountX = 65535;
      _resource.maxComputeWorkGroupCountY = 65535;
      _resource.maxComputeWorkGroupCountZ = 65535;
      _resource.maxComputeWorkGroupSizeX = 1024;
      _resource.maxComputeWorkGroupSizeY = 1024;
      _resource.maxComputeWorkGroupSizeZ = 64;
      _resource.maxComputeUniformComponents = 1024;
      _resource.maxComputeTextureImageUnits = 16;
      _resource.maxComputeImageUniforms = 8;
      _resource.maxComputeAtomicCounters = 8;
      _resource.maxComputeAtomicCounterBuffers = 1;
      _resource.maxVaryingComponents = 60;
      _resource.maxVertexOutputComponents = 64;
      _resource.maxGeometryInputComponents = 64;
      _resource.maxGeometryOutputComponents = 128;
      _resource.maxFragmentInputComponents = 128;
      _resource.maxImageUnits = 8;
      _resource.maxCombinedImageUnitsAndFragmentOutputs = 8;
      _resource.maxCombinedShaderOutputResources = 8;
      _resource.maxImageSamples = 0;
      _resource.maxVertexImageUniforms = 0;
      _resource.maxTessControlImageUniforms = 0;
      _resource.maxTessEvaluationImageUniforms = 0;
      _resource.maxGeometryImageUniforms = 0;
      _resource.maxFragmentImageUniforms = 8;
      _resource.maxCombinedImageUniforms = 8;
      _resource.maxGeometryTextureImageUnits = 16;
      _resource.maxGeometryOutputVertices = 256;
      _resource.maxGeometryTotalOutputComponents = 1024;
      _resource.maxGeometryUniformComponents = 1024;
      _resource.maxGeometryVaryingComponents = 64;
      _resource.maxTessControlInputComponents = 128;
      _resource.maxTessControlOutputComponents = 128;
      _resource.maxTessControlTextureImageUnits = 16;
      _resource.maxTessControlUniformComponents = 1024;
      _resource.maxTessControlTotalOutputComponents = 4096;
      _resource.maxTessEvaluationInputComponents = 128;
      _resource.maxTessEvaluationOutputComponents = 128;
      _resource.maxTessEvaluationTextureImageUnits = 16;
      _resource.maxTessEvaluationUniformComponents = 1024;
      _resource.maxTessPatchComponents = 120;
      _resource.maxPatchVertices = 32;
      _resource.maxTessGenLevel = 64;
      _resource.maxViewports = 16;
      _resource.maxVertexAtomicCounters = 0;
      _resource.maxTessControlAtomicCounters = 0;
      _resource.maxTessEvaluationAtomicCounters = 0;
      _resource.maxGeometryAtomicCounters = 0;
      _resource.maxFragmentAtomicCounters = 8;
      _resource.maxCombinedAtomicCounters = 8;
      _resource.maxAtomicCounterBindings = 1;
      _resource.maxVertexAtomicCounterBuffers = 0;
      _resource.maxTessControlAtomicCounterBuffers = 0;
      _resource.maxTessEvaluationAtomicCounterBuffers = 0;
      _resource.maxGeometryAtomicCounterBuffers = 0;
      _resource.maxFragmentAtomicCounterBuffers = 1;
      _resource.maxCombinedAtomicCounterBuffers = 1;
      _resource.maxAtomicCounterBufferSize = 16384;
      _resource.maxTransformFeedbackBuffers = 4;
      _resource.maxTransformFeedbackInterleavedComponents = 64;
      _resource.maxCullDistances = 8;
      _resource.maxCombinedClipAndCullDistances = 8;
      _resource.maxSamples = 4;
      _resource.limits.nonInductiveForLoops = 1;
      _resource.limits.whileLoops = 1;
      _resource.limits.doWhileLoops = 1;
      _resource.limits.generalUniformIndexing = 1;
      _resource.limits.generalAttributeMatrixVectorIndexing = 1;
      _resource.limits.generalVaryingIndexing = 1;
      _resource.limits.generalSamplerIndexing = 1;
      _resource.limits.generalVariableIndexing = 1;
      _resource.limits.generalConstantMatrixVectorIndexing = 1;
    }

    GLSLToSPIRVCompiler::~GLSLToSPIRVCompiler()
    {
    #ifndef __ANDROID__
      glslang::FinalizeProcess();
    #endif
    }


    void GLSLToSPIRVCompiler::LoadProgram( const glslang::TProgram& program,
      const vk::ShaderStageFlagBits& stageFlag )
    {
      for (int32_t i = program.getNumLiveUniformBlocks() - 1; i >= 0; i--)
      {
        LoadUniformBlock(program, stageFlag, i);
      }

      for (int32_t i = 0; i < program.getNumLiveUniformVariables(); i++)
      {
        LoadUniform(program, stageFlag, i);
      }

      for (int32_t i = 0; i < program.getNumLiveAttributes(); i++)
      {
        LoadVertexAttribute(program, stageFlag, i);
      }
    }


    void GLSLToSPIRVCompiler::LoadUniformBlock(const glslang::TProgram &program, 
      const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i)
    {
      for( auto& uniBlock: _uniformBlocks )
      {
        if( uniBlock->_name == program.getUniformBlockName( i ) )
        {
          uniBlock->_stageFlags = uniBlock->_stageFlags | stageFlag;
          return;
        }
      }

      UniformBlockType type = BLOCK_UNIFORM;

      if (strcmp(program.getUniformBlockTType(i)->getStorageQualifierString(), "buffer") == 0)
      {
        type = BLOCK_STORAGE;
      }

      if (program.getUniformBlockTType(i)->getQualifier().layoutPushConstant)
      {
        type = BLOCK_PUSH;
      }

      _uniformBlocks.emplace_back(
        std::make_unique<UniformBlock>(program.getUniformBlockName(i), 
          program.getUniformBlockBinding(i), program.getUniformBlockSize(i), 
          stageFlag, type));
    }

    void GLSLToSPIRVCompiler::LoadUniform(const glslang::TProgram &program, 
      const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i)
    {
      if (program.getUniformBinding(i) == -1)
      {

        auto splitName = StringUtils::split_str(
          program.getUniformName(i), "."
        );

        if (splitName.size() == 2)
        {
          for (auto &uniformBlock : _uniformBlocks)
          {
            if (uniformBlock->_name == splitName.at(0))
            {
              uniformBlock->addUniform(new Uniform(
                splitName.at(1), program.getUniformBinding(i), 
                program.getUniformBufferOffset(i),
                1, // TODO HARDCODED (recreated later) sizeof(float) * program.getUniformTType(i)->computeNumComponents(),
                program.getUniformType(i), stageFlag)
              );
              return;
            }
          }
        }
      }

      for (auto &uniform : _uniforms)
      {
        if (uniform->_name == program.getUniformName(i))
        {
          uniform->_stageFlags = (uniform->_stageFlags | stageFlag);
          return;
        }
      }

      std::cout << "Adding uniform" << std::endl;

      _uniforms.emplace_back(std::make_unique<Uniform>(
        program.getUniformName(i), program.getUniformBinding(i), 
        program.getUniformBufferOffset(i), -1, program.getUniformType(i), stageFlag)
      );
    }

    void GLSLToSPIRVCompiler::LoadVertexAttribute(const glslang::TProgram &program, 
        const vk::ShaderStageFlagBits &stageFlag, const uint32_t &i)
    {
      for (auto &vertexAttribute : _vertexAttributes)
      {
        if (vertexAttribute->_name == program.getAttributeName(i))
        {
          return;
        }
      }

      _vertexAttributes.emplace_back(
        std::make_unique<VertexAttribute>(
          program.getAttributeName(i), 
          program.getAttributeTType(i)->getQualifier().layoutLocation, 
          sizeof(float) * program.getAttributeTType(i)->getVectorSize(), // TODO
          program.getAttributeType(i)
        )
      );
    }

    bool cmp(const std::unique_ptr<Uniform> &a, const std::unique_ptr<Uniform> &b)
    {
      return a->_offset < b->_offset;
    }

    bool GLSLToSPIRVCompiler::compile(vk::ShaderStageFlagBits stage,
      const std::string& source, std::vector<uint32_t> &spirv)
    {
      static const std::map<vk::ShaderStageFlagBits, EShLanguage> stageToLanguageMap
      {
        {vk::ShaderStageFlagBits::eVertex, EShLangVertex},
        {vk::ShaderStageFlagBits::eTessellationControl, EShLangTessControl},
        {vk::ShaderStageFlagBits::eTessellationEvaluation, EShLangTessEvaluation},
        {vk::ShaderStageFlagBits::eGeometry, EShLangGeometry},
        {vk::ShaderStageFlagBits::eFragment, EShLangFragment},
        {vk::ShaderStageFlagBits::eCompute, EShLangCompute}
      };

      std::map<vk::ShaderStageFlagBits, EShLanguage>::const_iterator stageIt = stageToLanguageMap.find(stage);
      assert( stageIt != stageToLanguageMap.end());
      glslang::TShader shader(stageIt->second);

      const char *shaderStrings[1];
      shaderStrings[0] = source.c_str();
      shader.setStrings(shaderStrings, 1);

      // Enable SPIR-V and Vulkan rules when parsing GLSL
      EShMessages messages = (EShMessages)(EShMsgSpvRules | EShMsgVulkanRules);

      std::cout << "Parsing ... " << std::endl;
      if (!shader.parse(&_resource, 100, false, messages))
      {
        std::string infoLog = shader.getInfoLog();
        std::string infoDebugLog = shader.getInfoDebugLog();
        std::cout << "____________ PARSE ____________" << std::endl;
        std::cout << "infoLog: " << infoLog << std::endl;
        std::cout << "infoDebugLog: " << infoDebugLog << std::endl;
        return false;
      }
      std::cout << " ... OK " << std::endl;

      glslang::TProgram program;
      program.addShader(&shader);

      if (!program.link(messages))
      {
        std::string infoLog = program.getInfoLog();
        std::string infoDebugLog = program.getInfoDebugLog();
        std::cout << "____________ LINK ____________" << std::endl;
        std::cout << "infoLog: " << infoLog << std::endl;
        std::cout << "infoDebugLog: " << infoDebugLog << std::endl;
        return false;
      }

      program.buildReflection( );

      //program.dumpReflection( );

      LoadProgram( program, stage);

      ProcessShader( );

      glslang::GlslangToSpv(*program.getIntermediate(stageIt->second), spirv);

      return true;
    }

    void GLSLToSPIRVCompiler::ProcessShader( void )
    {
      // Sort uniforms by binding
      std::sort(_uniforms.begin(), _uniforms.end(),
        [](const std::unique_ptr<Uniform> &l, const std::unique_ptr<Uniform> &r)
      {
        return l->_binding < r->_binding;
      });

      // Sort uniform blocks by binding.
      std::sort(_uniformBlocks.begin(), _uniformBlocks.end(),
        [](const std::unique_ptr<UniformBlock> &l, const std::unique_ptr<UniformBlock> &r)
      {
        return l->_binding < r->_binding;
      });

      // Sort uniform block uniforms by offsets.
      for (auto &uniformBlock : _uniformBlocks)
      {
        std::sort(uniformBlock->_uniforms.begin(), uniformBlock->_uniforms.end(),
          [](const std::unique_ptr<Uniform> &l, const std::unique_ptr<Uniform> &r)
        {
          return l->_offset < r->_offset;
        });
      }




      // Fixing uniform size in uniform block
      for( auto& ub: _uniformBlocks )
      {
        for( int i = 0; i < ub->_uniforms.size( ); ++i )
        {
          if( i + 1 < ub->_uniforms.size( ) )
          {
            ub->_uniforms[ i ]->_size = ub->_uniforms[ i + 1 ]->_offset - ub->_uniforms[ i ]->_offset;
          }
          if( i + 1 == ub->_uniforms.size( ) )
          {
            ub->_uniforms[ i ]->_size = ub->_size - ub->_uniforms[ i ]->_offset;
          }
        }
      }

      // Sort attributes by location.
      std::sort(_vertexAttributes.begin(), _vertexAttributes.end(),
        [](const std::unique_ptr<VertexAttribute> &l, const std::unique_ptr<VertexAttribute> &r)
      {
        return l->_location < r->_location;
      });



      std::cout << "UNIFORM BLOCKS: " << std::endl;
      for( const auto& ub: _uniformBlocks )
      {
        std::cout << "\t" << ub->ToString( ) << std::endl;
      }

      std::cout << "UNIFORMS: " << std::endl;
      for( const auto& ub: _uniforms )
      {
        std::cout << "\t" << ub->ToString( ) << std::endl;
      }


      // Processing attribute descriptions
      std::cout << "VERTEX ATTRIB: " << std::endl;
      uint32_t currentOffset = 0;
      for( auto &va: _vertexAttributes )
      {
        std::cout << "\t" << va->ToString( ) <<
          " offset => " << currentOffset << std::endl;
        currentOffset += va->_size;
      }

      /*std::vector<vk::PushConstantRange> pushConstantRanges =;
      uint32_t currentOffset = 0;

      for ( const auto &ub : _uniformBlocks )
      {
        if ( ub->_type != BLOCK_PUSH)
        {
          continue;
        }

        vk::PushConstantRange pushConstantRange};
        pushConstantRange.stageFlags = ub->_stageFlags;
        pushConstantRange.offset = currentOffset;
        pushConstantRange.size = static_cast<uint32_t>(ub->_size);

        pushConstantRanges.emplace_back(pushConstantRange);

        currentOffset += pushConstantRange.size;
      }*/
    }

    bool GLSLtoSPV( vk::ShaderStageFlagBits stage,
      const std::string& source, std::vector<uint32_t> &spirv )
    {
      static GLSLToSPIRVCompiler compiler;
      return compiler.compile(stage, source, spirv);
    }
  } 
}