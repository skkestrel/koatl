from IPython.core.inputtransformer2 import (
    EscapedCommand,
    MagicAssign,
    SystemAssign,
    find_end_of_continued_line,
    assemble_continued_line,
)

import re

ESC_SHELL = "!"  # Send line to underlying system shell
ESC_SH_CAP = "!!"  # Send line to system shell and capture output
ESC_HELP = "?"  # Find information about object
ESC_HELP2 = "??"  # Find extra-detailed information about object
ESC_MAGIC = "%"  # Call magic function
ESC_MAGIC2 = "%%"  # Call cell-magic function
ESC_QUOTE = ","  # Split args on whitespace, quote each as string and call
ESC_QUOTE2 = ";"  # Quote all args as a single string, call
ESC_PAREN = "/"  # Call first argument with rest of line as arguments

ESCAPE_SINGLES = {"!", "?", "%", ",", ";", "/"}
ESCAPE_DOUBLES = {"!!", "??"}  # %% (cell magic) is handled separately


def _make_help_call(target, esc):
    """Prepares a pinfo(2)/psearch call from a target name and the escape
    (i.e. ? or ??)"""
    method = "pinfo2" if esc == "??" else "psearch" if "*" in target else "pinfo"
    arg = " ".join([method, target])
    # Prepare arguments for get_ipython().run_line_magic(magic_name, magic_args)
    t_magic_name, _, t_magic_arg_s = arg.partition(" ")
    t_magic_name = t_magic_name.lstrip(ESC_MAGIC)
    return "get_ipython().run_line_magic({}, {})".format(
        crepr(t_magic_name), crepr(t_magic_arg_s)
    )


def crepr(s):
    return repr(s)


def _tr_help(content):
    """Translate lines escaped with: ?

    A naked help line should fire the intro help screen (shell.show_usage())
    """
    if not content:
        return "get_ipython().show_usage()"

    return _make_help_call(content, "?")


def _tr_help2(content):
    """Translate lines escaped with: ??

    A naked help line should fire the intro help screen (shell.show_usage())
    """
    if not content:
        return "get_ipython().show_usage()"

    return _make_help_call(content, "??")


def _tr_magic(content):
    "Translate lines escaped with a percent sign: %"
    name, _, args = content.partition(" ")
    return "get_ipython().run_line_magic({}, {})".format(crepr(name), crepr(args))


def _tr_quote(content):
    "Translate lines escaped with a comma: ,"
    name, _, args = content.partition(" ")
    return '%s("%s")' % (name, '", "'.join(args.split()))


def _tr_quote2(content):
    "Translate lines escaped with a semicolon: ;"
    name, _, args = content.partition(" ")
    return '%s("%s")' % (name, args)


def _tr_paren(content):
    "Translate lines escaped with a slash: /"
    name, _, args = content.partition(" ")
    if name == "":
        raise SyntaxError(f'"{ESC_SHELL}" must be followed by a callable name')

    return "%s(%s)" % (name, ", ".join(args.split()))


tr = {
    ESC_SHELL: lambda s: "get_ipython().system({})".format(crepr(s)),
    ESC_SH_CAP: lambda s: "get_ipython().getoutput({})".format(crepr(s)),
    ESC_HELP: _tr_help,
    ESC_HELP2: _tr_help2,
    ESC_MAGIC: _tr_magic,
    ESC_QUOTE: _tr_quote,
    ESC_QUOTE2: _tr_quote2,
    ESC_PAREN: _tr_paren,
}


class KoatlEscapedCommand(EscapedCommand):
    def transform(self, lines):
        """Transform an escaped line found by the ``find()`` classmethod."""
        start_line, start_col = self.start_line, self.start_col

        indent = lines[start_line][:start_col]
        end_line = find_end_of_continued_line(lines, start_line)
        line = assemble_continued_line(lines, (start_line, start_col), end_line)

        if len(line) > 1 and line[:2] in ESCAPE_DOUBLES:
            escape, content = line[:2], line[2:]
        else:
            escape, content = line[:1], line[1:]

        if escape in tr:
            call = tr[escape](content)
        else:
            call = ""

        lines_before = lines[:start_line]
        new_line = indent + call + "\n"
        lines_after = lines[end_line + 1 :]

        return lines_before + [new_line] + lines_after


class KoatlMagicAssign(MagicAssign):
    priority = 9

    def transform(self, lines):
        start_line, start_col = self.start_line, self.start_col
        lhs = lines[start_line][:start_col]
        end_line = find_end_of_continued_line(lines, start_line)
        rhs = assemble_continued_line(lines, (start_line, start_col), end_line)
        assert rhs.startswith("%"), rhs
        magic_name, _, args = rhs[1:].partition(" ")

        lines_before = lines[:start_line]
        call = "get_ipython().run_line_magic({}, {})".format(
            crepr(magic_name), crepr(args)
        )
        new_line = lhs + call + "\n"
        lines_after = lines[end_line + 1 :]

        return lines_before + [new_line] + lines_after


class KoatlSystemAssign(SystemAssign):
    priority = 9

    def transform(self, lines):
        start_line, start_col = self.start_line, self.start_col

        lhs = lines[start_line][:start_col]
        end_line = find_end_of_continued_line(lines, start_line)
        rhs = assemble_continued_line(lines, (start_line, start_col), end_line)
        assert rhs.startswith("!"), rhs
        cmd = rhs[1:]

        lines_before = lines[:start_line]
        call = "get_ipython().getoutput({})".format(crepr(cmd))
        new_line = lhs + call + "\n"
        lines_after = lines[end_line + 1 :]

        return lines_before + [new_line] + lines_after


def koatl_cell_magic(lines):
    if not lines or not lines[0].startswith("%%"):
        return lines
    if re.match(r"%%\w+\?", lines[0]):
        # This case will be handled by help_end
        return lines
    magic_name, _, first_line = lines[0][2:].rstrip().partition(" ")
    body = "".join(lines[1:])
    return [
        "get_ipython().run_cell_magic({}, {}, {})\n".format(
            crepr(magic_name), crepr(first_line), crepr(body)
        )
    ]
