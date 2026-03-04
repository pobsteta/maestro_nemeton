"""Inspecter la structure de l'encodeur aerial MAESTRO."""
import types
import sys
import torch

mod_paths = [
    "maestro", "maestro.conf", "maestro.conf.mask",
    "maestro.conf.model", "maestro.conf.data",
    "maestro.conf.train", "maestro.conf.experiment",
    "maestro.model", "maestro.model.mae",
]

class GenericConfig:
    def __init__(self, *args, **kwargs):
        self.__dict__.update(kwargs)
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
sd = checkpoint["state_dict"]

# 1. Toutes les cles de l'encodeur aerial
print("=" * 60)
print("ENCODER AERIAL - toutes les cles:")
print("=" * 60)
aerial_keys = sorted([k for k in sd if k.startswith("model.encoder.aerial")])
for k in aerial_keys:
    print("  %s: %s" % (k, sd[k].shape))

# 2. Encoder inter
print("\n" + "=" * 60)
print("ENCODER INTER - toutes les cles:")
print("=" * 60)
inter_keys = sorted([k for k in sd if k.startswith("model.encoder_inter")])
for k in inter_keys:
    print("  %s: %s" % (k, sd[k].shape))

# 3. Heads
print("\n" + "=" * 60)
print("HEADS - toutes les cles:")
print("=" * 60)
head_keys = sorted([k for k in sd if k.startswith("model.heads")])
for k in head_keys:
    print("  %s: %s" % (k, sd[k].shape))

# 4. Deduire les parametres
print("\n" + "=" * 60)
print("PARAMETRES DEDUITS:")
print("=" * 60)
# embed_dim from norm
print("  embed_dim: %d" % sd["model.encoder.aerial.layers.0.0.norm.bias"].shape[0])
# num layers
layer_ids = set()
for k in aerial_keys:
    parts = k.split(".")
    if "layers" in parts:
        idx = parts.index("layers")
        if idx + 1 < len(parts):
            layer_ids.add(int(parts[idx + 1]))
print("  num_layers (aerial): %d" % (max(layer_ids) + 1))
# num heads from attn
for k in aerial_keys:
    if "attn" in k and "weight" in k:
        print("  attn weight shape: %s (%s)" % (sd[k].shape, k))
        break
# patch embed
pe = sd["model.patch_embed.aerial.patchify_bands.0.conv.weight"]
print("  patch_embed conv: in_ch=%d, patch=%dx%d, out=%d" % (
    pe.shape[1], pe.shape[2], pe.shape[3], pe.shape[0]))

# 5. Layers structure
print("\n" + "=" * 60)
print("ENCODER AERIAL - layer 0 detail:")
print("=" * 60)
for k in aerial_keys:
    if ".layers.0." in k:
        print("  %s: %s" % (k.replace("model.encoder.aerial.", ""), sd[k].shape))
