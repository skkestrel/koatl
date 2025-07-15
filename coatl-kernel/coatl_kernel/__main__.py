from ipykernel.kernelapp import IPKernelApp
from . import CoatlKernel

IPKernelApp.launch_instance(kernel_class=CoatlKernel)
