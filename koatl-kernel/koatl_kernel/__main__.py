from ipykernel.kernelapp import IPKernelApp
from . import KoatlKernel

IPKernelApp.launch_instance(kernel_class=KoatlKernel)
