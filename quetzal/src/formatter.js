/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
/* eslint-disable @typescript-eslint/ban-types */
import * as $wcm from '@vscode/wasm-component-model';
export var Types;
(function (Types) {
    let OhtliDiffTag;
    (function (OhtliDiffTag) {
        OhtliDiffTag["equal"] = "equal";
        OhtliDiffTag["delete"] = "delete";
        OhtliDiffTag["insert"] = "insert";
    })(OhtliDiffTag = Types.OhtliDiffTag || (Types.OhtliDiffTag = {}));
})(Types || (Types = {}));
export var formatter;
(function (formatter) {
    formatter.OhtliDiffTag = Types.OhtliDiffTag;
})(formatter || (formatter = {}));
(function (Types) {
    var $;
    (function ($) {
        $.OhtliDiffTag = new $wcm.EnumType(['equal', 'delete', 'insert']);
        $.OhtliDiffLine = new $wcm.RecordType([
            ['tag', $.OhtliDiffTag],
            ['value', $wcm.wstring],
        ]);
        $.OhtliDiffHunk = new $wcm.RecordType([
            ['tag', $.OhtliDiffTag],
            ['oldStart', $wcm.u32],
            ['oldCount', $wcm.u32],
            ['newStart', $wcm.u32],
            ['newCount', $wcm.u32],
            ['lines', new $wcm.ListType($.OhtliDiffLine)],
        ]);
    })($ = Types.$ || (Types.$ = {}));
})(Types || (Types = {}));
(function (Types) {
    var _;
    (function (_) {
        _.id = 'koatl:ohtli/types';
        _.witName = 'types';
        _.types = new Map([
            ['OhtliDiffTag', Types.$.OhtliDiffTag],
            ['OhtliDiffLine', Types.$.OhtliDiffLine],
            ['OhtliDiffHunk', Types.$.OhtliDiffHunk]
        ]);
    })(_ = Types._ || (Types._ = {}));
})(Types || (Types = {}));
(function (formatter) {
    var $;
    (function ($) {
        $.OhtliDiffHunk = Types.$.OhtliDiffHunk;
        $.OhtliDiffTag = Types.$.OhtliDiffTag;
        $.OhtliDiffLine = Types.$.OhtliDiffLine;
        let exports;
        (function (exports) {
            exports.format = new $wcm.FunctionType('format', [
                ['o', $wcm.wstring],
            ], $wcm.wstring);
            exports.formatDiff = new $wcm.FunctionType('format-diff', [
                ['original', $wcm.wstring],
            ], new $wcm.ListType($.OhtliDiffHunk));
        })(exports = $.exports || ($.exports = {}));
    })($ = formatter.$ || (formatter.$ = {}));
})(formatter || (formatter = {}));
(function (formatter) {
    var _;
    (function (_) {
        _.id = 'koatl:ohtli/formatter';
        _.witName = 'formatter';
        let imports;
        (function (imports) {
            imports.interfaces = new Map([
                ['Types', Types._]
            ]);
            function create(service, context) {
                return $wcm.$imports.create(_, service, context);
            }
            imports.create = create;
            function loop(service, context) {
                return $wcm.$imports.loop(_, service, context);
            }
            imports.loop = loop;
        })(imports = _.imports || (_.imports = {}));
        let exports;
        (function (exports_1) {
            exports_1.functions = new Map([
                ['format', formatter.$.exports.format],
                ['formatDiff', formatter.$.exports.formatDiff]
            ]);
            exports_1.interfaces = new Map([
                ['Types', Types._]
            ]);
            function bind(exports, context) {
                return $wcm.$exports.bind(_, exports, context);
            }
            exports_1.bind = bind;
        })(exports = _.exports || (_.exports = {}));
        function bind(service, code, portOrContext, context) {
            return $wcm.$main.bind(_, service, code, portOrContext, context);
        }
        _.bind = bind;
    })(_ = formatter._ || (formatter._ = {}));
})(formatter || (formatter = {}));
