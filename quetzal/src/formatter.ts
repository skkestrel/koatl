/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
/* eslint-disable @typescript-eslint/ban-types */
import * as $wcm from '@vscode/wasm-component-model';
import type { u32, i32, ptr } from '@vscode/wasm-component-model';

export namespace Types {
	export enum OhtliDiffTag {
		equal = 'equal',
		delete = 'delete',
		insert = 'insert'
	}

	export type OhtliDiffLine = {
		tag: OhtliDiffTag;
		value: string;
	};

	export type OhtliDiffHunk = {
		tag: OhtliDiffTag;
		oldStart: u32;
		oldCount: u32;
		newStart: u32;
		newCount: u32;
		lines: OhtliDiffLine[];
	};
}
export type Types = {
};
export namespace formatter {
	export type OhtliDiffHunk = Types.OhtliDiffHunk;
	export type OhtliDiffTag = Types.OhtliDiffTag;
	export const OhtliDiffTag = Types.OhtliDiffTag;
	export type OhtliDiffLine = Types.OhtliDiffLine;
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
		formatDiff: (original: string) => OhtliDiffHunk[];
	};
	export namespace Exports {
		export type Promisified = $wcm.$exports.Promisify<Exports>;
	}
	export namespace exports {
		export type Promisify<T> = $wcm.$exports.Promisify<T>;
	}
}

export namespace Types.$ {
	export const OhtliDiffTag = new $wcm.EnumType<Types.OhtliDiffTag>(['equal', 'delete', 'insert']);
	export const OhtliDiffLine = new $wcm.RecordType<Types.OhtliDiffLine>([
		['tag', OhtliDiffTag],
		['value', $wcm.wstring],
	]);
	export const OhtliDiffHunk = new $wcm.RecordType<Types.OhtliDiffHunk>([
		['tag', OhtliDiffTag],
		['oldStart', $wcm.u32],
		['oldCount', $wcm.u32],
		['newStart', $wcm.u32],
		['newCount', $wcm.u32],
		['lines', new $wcm.ListType<Types.OhtliDiffLine>(OhtliDiffLine)],
	]);
}
export namespace Types._ {
	export const id = 'koatl:ohtli/types' as const;
	export const witName = 'types' as const;
	export const types: Map<string, $wcm.AnyComponentModelType> = new Map<string, $wcm.AnyComponentModelType>([
		['OhtliDiffTag', $.OhtliDiffTag],
		['OhtliDiffLine', $.OhtliDiffLine],
		['OhtliDiffHunk', $.OhtliDiffHunk]
	]);
	export type WasmInterface = {
	};
}
export namespace formatter.$ {
	export const OhtliDiffHunk = Types.$.OhtliDiffHunk;
	export const OhtliDiffTag = Types.$.OhtliDiffTag;
	export const OhtliDiffLine = Types.$.OhtliDiffLine;
	export namespace exports {
		export const format = new $wcm.FunctionType<formatter.Exports['format']>('format',[
			['o', $wcm.wstring],
		], $wcm.wstring);
		export const formatDiff = new $wcm.FunctionType<formatter.Exports['formatDiff']>('format-diff',[
			['original', $wcm.wstring],
		], new $wcm.ListType<formatter.OhtliDiffHunk>(OhtliDiffHunk));
	}
}
export namespace formatter._ {
	export const id = 'koatl:ohtli/formatter' as const;
	export const witName = 'formatter' as const;
	export namespace imports {
		export const interfaces: Map<string, $wcm.InterfaceType> = new Map<string, $wcm.InterfaceType>([
			['Types', Types._]
		]);
		export function create(service: formatter.Imports, context: $wcm.WasmContext): Imports {
			return $wcm.$imports.create<Imports>(_, service, context);
		}
		export function loop(service: formatter.Imports, context: $wcm.WasmContext): formatter.Imports {
			return $wcm.$imports.loop<formatter.Imports>(_, service, context);
		}
	}
	export type Imports = {
	};
	export namespace exports {
		export const functions: Map<string, $wcm.FunctionType> = new Map([
			['format', $.exports.format],
			['formatDiff', $.exports.formatDiff]
		]);
		export const interfaces: Map<string, $wcm.InterfaceType> = new Map<string, $wcm.InterfaceType>([
			['Types', Types._]
		]);
		export function bind(exports: Exports, context: $wcm.WasmContext): formatter.Exports {
			return $wcm.$exports.bind<formatter.Exports>(_, exports, context);
		}
	}
	export type Exports = {
		'format': (o_ptr: i32, o_len: i32, result: ptr<string>) => void;
		'format-diff': (original_ptr: i32, original_len: i32, result: ptr<OhtliDiffHunk[]>) => void;
	};
	export function bind(service: formatter.Imports, code: $wcm.Code, context?: $wcm.ComponentModelContext): Promise<formatter.Exports>;
	export function bind(service: formatter.Imports.Promisified, code: $wcm.Code, port: $wcm.RAL.ConnectionPort, context?: $wcm.ComponentModelContext): Promise<formatter.Exports.Promisified>;
	export function bind(service: formatter.Imports | formatter.Imports.Promisified, code: $wcm.Code, portOrContext?: $wcm.RAL.ConnectionPort | $wcm.ComponentModelContext, context?: $wcm.ComponentModelContext | undefined): Promise<formatter.Exports> | Promise<formatter.Exports.Promisified> {
		return $wcm.$main.bind(_, service, code, portOrContext, context);
	}
}