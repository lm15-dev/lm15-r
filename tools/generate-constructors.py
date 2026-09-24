#!/usr/bin/env python3
"""Generate R constructors from the package's canonical schema declarations.

No reference runtime, fixtures, network requests, or tests are executed.
"""
import pathlib
import re

root = pathlib.Path(__file__).resolve().parents[1]
source = (root / 'R/schema.R').read_text().split('.unions <-')[0]
shapes = []
for line in source.splitlines():
    m = re.match(r'  (\w+) = (?:c\((.*)\)|character\(\)),?$', line)
    if m:
        shapes.append((m[1], re.findall(r'(\w+) = "([^"]+)"', m[2] or '')))

def snake(name):
    return re.sub(r'([a-z0-9])([A-Z])', r'\1_\2', name).lower()

required_arrays = {('Request', 'messages'), ('Message', 'parts'), ('ToolResultPart', 'content'),
                   ('BatchRequest', 'requests'), ('ImageGenerationResponse', 'images'),
                   ('LiveClientTurnEvent', 'parts'), ('LiveClientToolResultEvent', 'content')}
media = dict(ImagePart='image/png', AudioPart='audio/wav', VideoPart='video/mp4',
             DocumentPart='application/pdf', BinaryPart='application/octet-stream',
             FileUploadRequest='application/octet-stream', LiveClientAudioEvent='audio/pcm;rate=16000',
             LiveClientImageEvent='image/jpeg')

def default(t, name, desc):
    if (t, name) in required_arrays:
        return None
    if desc.endswith('[]'):
        return 'list("text")' if t == 'InferenceModelInfo' and name in ('input_modalities', 'output_modalities') else 'list()'
    if desc.endswith('?'):
        return 'NULL'
    if name == 'part_index': return '0L'
    if name in ('is_error', 'supports_reasoning'): return 'FALSE'
    if name == 'turn_complete': return 'TRUE'
    if name == 'channels': return '1L'
    if name == 'readiness': return '"ready"'
    if name == 'currency': return '"USD"'
    if t == 'ModelOrigin' and name == 'type': return '"provider"'
    if name == 'mode': return '"auto"'
    if name == 'parameters': return 'json_object(type = "object", properties = json_object())'
    if name in ('data', 'input') and desc == 'object': return 'json_object()'
    if name == 'config' and desc == 'Config': return '.new_value("Config", list())'
    if name == 'usage' and desc == 'Usage': return '.new_value("Usage", list())'
    if name == 'origin': return '.new_value("ModelOrigin", list())'
    if name == 'media_type' and t in media: return repr(media[t]).replace("'", '"')
    if name == 'message' and t == 'ErrorDetail': return '""'
    return None

out = ['# Generated from schema.R by tools/generate-constructors.py.\n']
exports = []
for t, fields in shapes:
    fn = snake(t)
    exports.append(fn)
    args = []
    required = [(n, d) for n, d in fields if default(t, n, d) is None]
    optional = [(n, d) for n, d in fields if default(t, n, d) is not None]
    for name, desc in required:
        args.append(name)
    args.append('...')
    for name, desc in optional:
        args.append(f'{name} = {default(t, name, desc)}')
    values = ', '.join(f'{n} = {n}' for n, _ in fields)
    out.append(f'{fn} <- function({", ".join(args)}) {{\n  .check_dots(...)\n  .new_value("{t}", list({values}))\n}}\n')
(root / 'R/constructors.R').write_text('\n'.join(out))
(root / 'tools/constructor-exports.txt').write_text('\n'.join(exports) + '\n')
