(Prism.languages.koatl = {
	comment: [
		{ pattern: /#-[\s\S]*?(?:-#|$)/, lookbehind: !0, greedy: !0 },
		{ pattern: /#[^-].*/, lookbehind: !0, greedy: !0 },
	],
	"string-interpolation": {
		pattern: /(?:f|fr|rf)(?:("""|''')[\s\S]*?\1|("|')(?:\\.|(?!\2)[^\\\r\n])*\2)/i,
		greedy: !0,
		inside: {
			interpolation: {
				pattern: /((?:^|[^{])(?:\{\{)*)\{(?!\{)(?:[^{}]|\{(?!\{)(?:[^{}]|\{(?!\{)(?:[^{}])+\})+\})+\}/,
				lookbehind: !0,
				inside: {
					"format-spec": { pattern: /(:)[^:(){}]+(?=\}$)/, lookbehind: !0 },
					"conversion-option": { pattern: /![sra](?=[:}]$)/, alias: "punctuation" },
					rest: null,
				},
			},
			string: /[\s\S]+/,
		},
	},
	"triple-quoted-string": { pattern: /(?:[rub]|br|rb)?("""|''')[\s\S]*?\1/i, greedy: !0, alias: "string" },
	string: { pattern: /(?:[rub]|br|rb)?("|')(?:\\.|(?!\1)[^\\\r\n])*\1/i, greedy: !0 },
	function: { pattern: /((?:^|\s)def[ \t]+)[a-zA-Z_]\w*(?=\s*\()/g, lookbehind: !0 },
	"class-name": { pattern: /(\bclass\s+)\w+/i, lookbehind: !0 },
	decorator: {
		pattern: /(^[\t ]*)@\w+(?:\.\w+)*/m,
		lookbehind: !0,
		alias: ["annotation", "punctuation"],
		inside: { punctuation: /\./ },
	},
	keyword:
		/\b(?:_(?=\s*:)|memo|let|const|export|matches|default|then|and|as|assert|break|case|class|continue|else|except|exec|finally|for|from|global|if|import|in|is|match|not|or|pass|print|raise|return|try|while|with|yield)\b/,
	builtin:
		/\b(?:__import__|Async|Env|Memo|Ok|Err|Result|Record|abs|callable|classmethod|complex|dict|eval|float|format|frozenset|getattr|globals|hasattr|hash|help|hex|int|iter|len|list|locals|long|max|min|next|object|property|repr|round|set|setattr|staticmethod|str|super|tuple|type)\b/,
	boolean: /\b(?:False|None|True)\b/,
	number: /\b0(?:b(?:_?[01])+|o(?:_?[0-7])+|x(?:_?[a-f0-9])+)\b|(?:\b\d+(?:_\d+)*(?:\.(?:\d+(?:_\d+)*)?)?|\B\.\d+(?:_\d+)*)(?:e[+-]?\d+(?:_\d+)*)?j?(?!\w)/i,
	operator: /[-+%=]=?|!=|:=|\*\*?=?|\/\/?=?|<[<=>]?|>[=>]?|[&|^~]/,
	punctuation: /[{}[\];(),.:]/,
}),
	(Prism.languages.koatl["string-interpolation"].inside.interpolation.inside.rest = Prism.languages.koatl);
