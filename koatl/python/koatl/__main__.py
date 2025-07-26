import sys
import subprocess
import os
import time


def main():
    mode = "script"
    transpile_only = False

    while True and len(sys.argv) > 1:
        if sys.argv[1] == "--help":
            print(
                f"Usage: python -m koatl [--trans] [--mode script|module|interactive] <filename.(py|tl)>"
            )
            sys.exit(1)

        if sys.argv[1] == "--mode":
            del sys.argv[1]
            mode = sys.argv[1]
            del sys.argv[1]
            continue

        elif sys.argv[1] == "--trans":
            del sys.argv[1]
            transpile_only = True
            continue

        break

    if len(sys.argv) < 2:
        from jupyter_client import KernelManager

        km = None

        try:
            km = KernelManager(kernel_name="koatl")
            km.start_kernel()

            # Give the kernel a moment to fully initialize
            # time.sleep(1)

            jupyter_command = [
                sys.executable,
                "-m",
                "jupyter_console",
                "--existing",
                km.connection_file,
            ]
            subprocess.call(jupyter_command)
        except FileNotFoundError:
            print("\nError: 'jupyter' command not found.", file=sys.stderr)
            print(
                "Please ensure 'jupyter_console' is installed (pip install jupyter_console) and in your PATH.",
                file=sys.stderr,
            )
            print(
                "Also, ensure 'koatl' kernel is installed for Jupyter.", file=sys.stderr
            )
            sys.exit(1)
        except Exception as e:
            print(f"\nAn unexpected error occurred: {e}", file=sys.stderr)
            print(
                "Please ensure the 'koatl' kernel is correctly installed and registered with Jupyter.",
                file=sys.stderr,
            )
            sys.exit(1)
        finally:
            # 4. Clean up: Shutdown the kernel and remove its connection file
            if km and km.is_alive():
                print("Shutting down the kernel...")
                km.shutdown_kernel(now=True)  # Forceful shutdown
                print("Kernel shut down.")

            # Clean up the connection file if it still exists
            if km and km.connection_file and os.path.exists(km.connection_file):
                try:
                    os.remove(km.connection_file)
                    print(f"Removed connection file: {km.connection_file}")
                except OSError as e:
                    print(
                        f"Error removing connection file {km.connection_file}: {e}",
                        file=sys.stderr,
                    )
    else:
        script_path = sys.argv[1]
        sys.argv = sys.argv[1:]

        try:
            with open(script_path, "r") as f:
                original_script_code = f.read()
        except FileNotFoundError:
            print(f"Error: File not found at '{script_path}'")
            sys.exit(1)

        if script_path.endswith(".tl"):
            import koatl.cli

            if transpile_only:
                print(
                    koatl.cli.transpile_from_source(
                        original_script_code, mode=mode, script_path=script_path
                    )
                )
            else:
                koatl.cli.run_from_source(
                    original_script_code, mode=mode, script_path=script_path
                )
        else:
            injected_code = """
        # -- injected code --
        from koatl.runtime import *
        """

            full_code = injected_code + original_script_code
            code_obj = compile(full_code, script_path, "exec")

            script_globals = {
                "__name__": "__main__",
            }

            exec(code_obj, script_globals)


if __name__ == "__main__":
    main()
