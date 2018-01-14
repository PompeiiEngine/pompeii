Lava 
=====================================================

# Introduction
Lava is a library built to facilitate the use of Vulkan features.

# Dependencies

* Required dependencies:
    * Vulkan
    * GLFW3 for window system
    * GLM for mathematics
    * STBI for texture reading
    * ASSIMP for mesh loading

* Optional dependencies:
	* GLSLANG

## Building

## <img src="./images/windowslogo.png" alt="" height="32px"> Windows

Use the provided CMakeLists.txt with [CMake](https://cmake.org) to generate a build configuration for your favorite IDE or compiler, e.g.:
```
git clone https://github.com/maldicion069/Lava.git
cd Lava
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -G "Visual Studio 14 2015 Win64"
```
## <img src="./images/linuxlogo.png" alt="" height="32px"> Linux

Use the provided CMakeLists.txt with [CMake](https://cmake.org) to generate a build configuration for your favorite IDE or compiler.
Lava has been successfully built and used on Ubuntu 16.04 LTS.

```bash
git clone https://github.com/maldicion069/Lava.git
cd Lava
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

## Notes
* You have to take the following issues into consideration:
	* Your GPU must support Vulkan. Download the drivers here: https://developer.nvidia.com/vulkan-driver.
	* Download the LunarG Vulkan SDK: https://vulkan.lunarg.com/sdk/home. Lava works with version 1.0.57.0.
	* You must have at least glfw version 3.3.0: https://github.com/glfw/glfw (branch master).
	* For GLM: ```sudo apt-get install libglm-dev```
	* For ASSIMP: ```sudo apt-get install libassimp-dev```

# Examples
You can check the demos in the [examples directory](examples).

# Contributors
* Cristian Rodríguez Bernal
* Juan Guerrero Martín 
* Gonzalo Bayo

# License

GNU GPL

**Free Software, Hell Yeah!**

