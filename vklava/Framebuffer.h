#pragma once

#include "includes.hpp"

#include "VulkanResource.h"
#include "noncopyable.hpp"

namespace lava
{
	class Device;
	class RenderPass;
	class ImageView;
	class Framebuffer: public VulkanResource, private NonCopyable<Framebuffer>
	{
	public:
		Framebuffer(const std::shared_ptr<Device>& device, const std::shared_ptr<RenderPass>& renderPass, 
			const std::vector<std::shared_ptr<ImageView>>& attachments, const vk::Extent2D& extent, uint32_t layers);
		virtual ~Framebuffer();

		inline operator vk::Framebuffer() const
		{
			return _framebuffer;
		}

	private:
		std::shared_ptr<RenderPass> _renderPass;
		vk::Framebuffer _framebuffer;
		std::vector<std::shared_ptr<ImageView>> _attachments;
	};
}