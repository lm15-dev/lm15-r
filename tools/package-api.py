#!/usr/bin/env python3
"""Write the package NAMESPACE from its public functions and S3 methods."""
import pathlib
import re
root = pathlib.Path(__file__).resolve().parents[1]
exports = set()
methods = []
for path in sorted((root / 'R').glob('*.R')):
    source = path.read_text()
    exports.update(re.findall(r'^([a-z][a-z0-9_]*) <- function\(', source, re.M))
    for generic, cls in re.findall(r'^(print|str|format|conditionMessage|Ops|as\.double|as\.integer|as\.character|complete|stream)\.([A-Za-z0-9_]+) <- function\(', source, re.M):
        methods.append((generic, cls))
exports -= {'text'}  # would mask graphics::text; text_part() is the exported constructor
methods += [('$', 'lm15_value'), ('$<-', 'lm15_value'), ('[[<-', 'lm15_value'), ('[<-', 'lm15_value'), ('$', 'lm15_json_object')]
namespace = ['# Written by tools/package-api.py.', 'importFrom(utils,str)', 'importFrom(stats,setNames)', 'useDynLib(lm15, .registration=TRUE)']
namespace += [f'export({name})' for name in sorted(exports)]
namespace += [f'S3method("{generic}",{cls})' for generic, cls in sorted(set(methods))]
(root / 'NAMESPACE').write_text('\n'.join(namespace) + '\n')
# The reference manual is written by tools/document.R from tools/reference.R.
