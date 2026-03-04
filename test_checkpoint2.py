"""Script pour inspecter les details du checkpoint MAESTRO."""
import types
import sys
import torch

# Memes stubs que test_checkpoint.py
mod_paths = [
    "maestro", "maestro.conf", "maestro.conf.mask",
    "maestro.conf.model", "maestro.conf.data",
    "maestro.conf.train", "maestro.conf.experiment",
    "maestro.model", "maestro.model.mae",
]

class GenericConfig:
    def __init__(self, *args, **kwargs):
        self.__dict__.update(kwargs)
    def __repr__(self):
        return "GenericConfig(%s)" % self.__dict__
    def __setstate__(self, state):
        if isinstance(state, dict):
            self.__dict__.update(state)

for mod_path in mod_paths:
    if mod_path not in sys.modules:
        m = types.ModuleType(mod_path)
        sys.modules[mod_path] = m
        parts = mod_path.rsplit(".", 1)
        if len(parts) == 2 and parts[0] in sys.modules:
            setattr(sys.modules[parts[0]], parts[1], m)

for mod_path in mod_paths:
    mod = sys.modules[mod_path]
    for attr in ["MaskConfig", "ModelConfig", "DataConfig", "TrainConfig",
                 "ExperimentConfig", "Config", "MAE", "MaskedAutoencoder"]:
        setattr(mod, attr, GenericConfig)

BLOB = (
    r"C:\Users\pascal.obstetar\.cache\huggingface\hub"
    r"\models--IGNF--MAESTRO_FLAIR-HUB_base\blobs"
    r"\2cc697d069bd6fe089e7e9e392cd7c810227a145a311107e493c9c7b7385e016"
)

checkpoint = torch.load(BLOB, map_location="cpu", weights_only=False)

# 1. Hyper-parameters
print("=" * 60)
print("HYPER-PARAMETERS:")
print("=" * 60)
hp = checkpoint["hyper_parameters"]
for k, v in hp.items():
    print("  %s: %s" % (k, v))

# 2. State dict key prefixes
print("\n" + "=" * 60)
print("STATE_DICT: %d keys total" % len(checkpoint["state_dict"]))
print("=" * 60)

# Group keys by prefix (first 3 levels)
prefixes = {}
for key in sorted(checkpoint["state_dict"].keys()):
    parts = key.split(".")
    prefix = ".".join(parts[:3])
    if prefix not in prefixes:
        prefixes[prefix] = []
    prefixes[prefix].append(key)

for prefix in sorted(prefixes.keys()):
    keys = prefixes[prefix]
    first_key = keys[0]
    shape = checkpoint["state_dict"][first_key].shape
    print("  %s (%d keys) - ex: %s %s" % (prefix, len(keys), first_key, shape))

# 3. Print all unique top-level prefixes
print("\n" + "=" * 60)
print("TOP-LEVEL PREFIXES:")
print("=" * 60)
top = set()
for key in checkpoint["state_dict"].keys():
    top.add(".".join(key.split(".")[:2]))
for t in sorted(top):
    print("  ", t)

# 4. Encoder block shapes (first block)
print("\n" + "=" * 60)
print("ENCODER BLOCK 0 SHAPES:")
print("=" * 60)
for key in sorted(checkpoint["state_dict"].keys()):
    if "blocks.0." in key and "encoder" in key:
        print("  %s: %s" % (key, checkpoint["state_dict"][key].shape))

# 5. Patch embed shapes
print("\n" + "=" * 60)
print("PATCH EMBED SHAPES:")
print("=" * 60)
for key in sorted(checkpoint["state_dict"].keys()):
    if "patch_embed" in key:
        print("  %s: %s" % (key, checkpoint["state_dict"][key].shape))
