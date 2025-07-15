from ipykernel.kernelbase import Kernel

class CoatlKernel(Kernel):
    implementation = 'Coatl'
    implementation_version = '1.0'
    language = 'coatl'
    language_version = '0.1'
    language_info = {
        'name': 'coatl',
        'mimetype': 'text/x-coatl',
        'file_extension': '.tl',
    }
    banner = "Coatl Kernel - Echo Mode"

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        if not silent:
            stream_content = {'name': 'stdout', 'text': code}
            self.send_response(self.iopub_socket, 'stream', stream_content)

        return {'status': 'ok',
                # The base class increments the execution count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }
