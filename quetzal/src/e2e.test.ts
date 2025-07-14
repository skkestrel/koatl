import { glob } from "glob";
import util from "util";
import { exec } from "child_process";

const execAsync = util.promisify(exec);

declare global {
	namespace jest {
		interface Matchers<R> {
			e2e_fail(): R;
		}
	}
}

expect.extend({
	e2e_fail(received) {
		return {
			message: () => `Non-zero exit code.\n${received.stderr}`,
			pass: false,
		};
	},
});

async function run_e2e(file: string) {
	try {
		await execAsync(`PYTHONPATH=tests/e2e ../target/release/coatl run ${file}`);
	} catch (error: any) {
		expect(error).e2e_fail();
	}
}

describe("e2e", () => {
	const files: string[] = [];

	for (const f of glob.iterateSync("tests/e2e/*.tl")) {
		files.push(f);
	}

	test.each(files)("%p", run_e2e);
});
