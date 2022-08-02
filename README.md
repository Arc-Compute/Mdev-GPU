# Mdev GPU

Mdev-GPU is a user-configurable utility for GPU vendor drivers enabling the registration of arbitrary mdev types with the VFIO-Mediated Device framework.

Mdev-GPU is released under GPLv2 as one component of the [GPU Virtual Machine (GVM) Project](https://gvm-docs.openmdev.io/).

# How do I use Mdev-GPU?

- [Documentation for Mdev-GPU](https://openmdev.io/index.php/Mdev-GPU)

# What does Mdev-GPU do?

GPU virtualization is not widely supported using free(libre) driver programs on commodity GPUs.

Mdev-GPU enables the creation of arbitrary user-configurable Mediated Device types on existing drivers which either don't support [VFIO-Mdev functionality](https://openmdev.io/index.php/Virtual_I/O_Internals#Mdev_Mode) or only support pre-defined Mdev types.

![](https://openmdev.io/images/c/cb/Mdev-GPU-Example_Use.gif)

# How does this work?

In order to virtualize the GPU via the VFIO-Mdev API a driver must register devices and mdev callbacks with the Mediated Core.
This may be accomplished via the vendor driver in the case that it supports pre-defined mdev types or via GVM/Mdev-GPU for drivers which do not include support for Mdev functionality and/or do not support arbitrary Mdev types.

A diagram below depicts the components Mdev-GPU interacts with.

![](https://openmdev.io/images/7/75/Mdev-gpu.png)

For further information see the Virtual I/O Internals article on OpenMdev.io under the section titled [Registering PCIE Device with the Mediated Core](https://openmdev.io/index.php/Virtual_I/O_Internals#Registering_PCIE_Device_with_the_Mediated_Core).

# What programs work with Mdev-GPU?

If you are running a program which interacts with the [VFIO-Mdev API](https://git.kernel.org/pub/scm/linux/kernel/git/gregkh/driver-core.git/tree/Documentation/driver-api/vfio-mediated-device.rst) then you can install Mdev-GPU underneath it without modifying your programs or changing your workflow.

Here is a list of some existing programs that can interact with functionality enabled by Mdev-GPU without modification:

- [LibVirt](https://libvirt.org/)
- [LibVF.IO](https://libvf.io/)
- [mdevctl](https://github.com/mdevctl/mdevctl)

# Learn More

- [Documentation from the OpenMdev Foundation](https://openmdev.io)

# Compilation

To compile this program run the following commands:

`curl -sSL https://get.haskellstack.org/ | sh`

`stack install`

The resulting binary file or files (depending on which GVM Project sources are compiled) will be located in ~/.local/bin/

This program can take some time to compile (around 10 minutes depending on your system) so if you would prefer to use a pre-built binary instead they are made available under [Releases](https://github.com/Arc-Compute/Mdev-GPU/releases).
