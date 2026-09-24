#!/usr/bin/env python3
"""Copy declarative provider tables without importing or running the reference."""
import ast
import json
import pathlib

root = pathlib.Path(__file__).resolve().parents[1]
ref = root.parent / 'lm15-python/lm15'
env = {}
constructors = {'OpenAIResponsesCompat', 'OpenAIChatCompat', 'AnthropicCompat', 'AccessPolicy', 'EndpointSupport', 'HostSpec', 'HostSetting'}
env.update(CLAUDE_CODE_LOGIN_HINT='Run claude and use /login.', OPENAI_CODEX_LOGIN_HINT='Run codex login.', XAI_LOGIN_HINT='Use login("xai") to sign in again.')
def evaluate(node):
    if isinstance(node, ast.Constant): return node.value
    if isinstance(node, ast.Name): return env[node.id]
    if isinstance(node, (ast.Tuple, ast.List)): return [evaluate(x) for x in node.elts]
    if isinstance(node, ast.Dict):
        out = {}
        for k, v in zip(node.keys, node.values):
            if k is None: out.update(evaluate(v))
            else: out[evaluate(k)] = evaluate(v)
        return out
    if isinstance(node, ast.Subscript): return evaluate(node.value)[evaluate(node.slice)]
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add): return evaluate(node.left) + evaluate(node.right)
    if isinstance(node, ast.JoinedStr): return ''.join(str(evaluate(x.value)) if isinstance(x, ast.FormattedValue) else x.value for x in node.values)
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == 'MappingProxyType':
        return evaluate(node.args[0])
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id in constructors:
        out = {k.arg: evaluate(k.value) for k in node.keywords}
        if node.func.id == 'HostSetting' and node.args: out['name'] = evaluate(node.args[0])
        return out
    raise ValueError(ast.dump(node))

for file in ('compat.py', 'access.py', 'router.py'):
    tree = ast.parse((ref / file).read_text())
    for node in tree.body:
        if isinstance(node, ast.AnnAssign) and node.value is not None: target, value = node.target, node.value
        elif isinstance(node, ast.Assign) and len(node.targets) == 1: target, value = node.targets[0], node.value
        else: continue
        try: val = evaluate(value)
        except (ValueError, KeyError): continue
        if isinstance(target, ast.Name): env[target.id] = val
        elif isinstance(target, ast.Subscript):
            try: evaluate(target.value)[evaluate(target.slice)] = val
            except (ValueError, KeyError): pass

own = {'openai': ('OPENAI_API', 'openai'), 'openai-chat': ('OPENAI_CHAT_API', 'openai'),
       'anthropic': ('ANTHROPIC_API', 'anthropic'), 'gemini': ('GEMINI_API', None),
       'xai': ('XAI', 'xai'), 'claude-code': ('CLAUDE_CODE', 'anthropic'), 'openai-codex': ('OPENAI_CODEX', 'openai')}
registry = []
for node in ast.walk(ast.parse((ref / 'registry.py').read_text())):
    if not isinstance(node, ast.AnnAssign) or not isinstance(node.target, ast.Name) or node.target.id != '_DEFINITIONS': continue
    for call in node.value.elts:
        fn = call.func.id
        kw = {k.arg: evaluate(k.value) for k in call.keywords}
        if fn == '_adapter_owned':
            id_, dialect = evaluate(call.args[0]), evaluate(call.args[1])
            if id_ not in own: continue  # a provider with a dialect of its own that R does not implement (typesafe)
            key, compat = own[id_]; policy = dict(env[key])
        else:
            policy = dict(env[call.args[0].attr]); id_ = policy['provider']
            dialect = {'_chat_bound': 'openai-chat', '_responses_bound': 'openai-responses', '_anthropic_bound': 'anthropic'}.get(fn)
            if fn == '_hosted': dialect = evaluate(call.args[1])
            compat = kw.get('compat') or (id_ if fn == '_chat_bound' else None)
        policy.setdefault('credential_policy', 'key')
        policy.setdefault('auth_scheme', ['bearer'])
        policy.setdefault('headers', [])
        policy.setdefault('env_keys', [])
        policy.setdefault('backend', 'api')
        policy.setdefault('backend_options', {})
        registry.append(dict(id=id_, dialect=dialect, compat=compat, access=policy, placeholder_key=kw.get('placeholder_key'), console_url=kw.get('console_url')))

out = {'providers': registry, 'chat_model_prefixes': env['LITELLM_PROVIDER_PREFIXES'], 'chat_client_keywords': env['_CLIENT_KEYWORDS']}
for dialect, prefix in [('responses', 'OPENAI_RESPONSES'), ('chat', 'OPENAI_CHAT'), ('anthropic', 'ANTHROPIC')]:
    out[dialect] = env[prefix + '_PRESETS']
    out[dialect + '_urls'] = env[prefix + '_PRESET_BASE_URLS']
    if not out[dialect]: raise ValueError('empty preset table')
text = json.dumps(out, ensure_ascii=True, separators=(',', ':'))
# R string literal: escape JSON backslashes and quotes one additional time.
literal = json.dumps(text, ensure_ascii=True)
(root / 'R/provider-data.R').write_text('# Declarative tables copied by tools/copy-provider-tables.py; no reference runtime needed.\n.provider_table_json <- ' + literal + '\n')
