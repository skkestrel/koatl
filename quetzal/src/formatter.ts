/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
/* eslint-disable @typescript-eslint/ban-types */
import * as $wcm from '@vscode/wasm-component-model';
import type { i32, ptr } from '@vscode/wasm-component-model';

export namespace formatter {
	export type Imports = {
	};
	export namespace Imports {
		export type Promisified = $wcm.$imports.Promisify<Imports>;
	}
	export namespace imports {
		export type Promisify<T> = $wcm.$imports.Promisify<T>;
	}
	export type Exports = {
		format: (o: string) => string;
	};
	export namespace Exports {
		export type Promisified = $wcm.$exports.Promisify<Exports>;
	}
	export namespace exports {
		export type Promisify<T> = $wcm.$exports.Promisify<T>;
	}
}

export namespace formatter.$ {
	export namespace exports {
		export const format = new $wcm.FunctionType<formatter.Exports['format']>('format',[
			['o', $wcm.wstring],
		], $wcm.wstring);
	}
}
export namespace formatter._ {
	export const id = 'koatl:ohtli/formatter' as const;
	export const witName = 'formatter' as const;
	export namespace exports {
		export const functions: Map<string, $wcm.FunctionType> = new Map([
			['format', $.exports.format]
		]);
		export function bind(exports: Exports, context: $wcm.WasmContext): formatter.Exports {
			return $wcm.$exports.bind<formatter.Exports>(_, exports, context);
		}
	}
	export type Exports = {
		'format': (o_ptr: i32, o_len: i32, result: ptr<string>) => void;
	};
	export function bind(service: formatter.Imports, code: $wcm.Code, context?: $wcm.ComponentModelContext): Promise<formatter.Exports>;
	export function bind(service: formatter.Imports.Promisified, code: $wcm.Code, port: $wcm.RAL.ConnectionPort, context?: $wcm.ComponentModelContext): Promise<formatter.Exports.Promisified>;
	export function bind(service: formatter.Imports | formatter.Imports.Promisified, code: $wcm.Code, portOrContext?: $wcm.RAL.ConnectionPort | $wcm.ComponentModelContext, context?: $wcm.ComponentModelContext | undefined): Promise<formatter.Exports> | Promise<formatter.Exports.Promisified> {
		return $wcm.$main.bind(_, service, code, portOrContext, context);
	}
}