"""Test de chargement du modele MAESTRO avec la nouvelle architecture."""
import torch
from maestro_inference import charger_modele

BLOB = (
    r"C:\Users\pascal.obstetar\.cache\huggingface\hub"
    r"\models--IGNF--MAESTRO_FLAIR-HUB_base\blobs"
    r"\2cc697d069bd6fe089e7e9e392cd7c810227a145a311107e493c9c7b7385e016"
)

print("Chargement du modele MAESTRO...")
modele = charger_modele(BLOB, n_classes=13, device="cpu", in_channels=4)

print("\nTest forward pass avec image aleatoire (1, 4, 256, 256)...")
dummy = torch.randn(1, 4, 256, 256)
with torch.no_grad():
    logits = modele(dummy)
print("  Output shape:", logits.shape)
print("  Logits:", logits[0].tolist())

probs = torch.softmax(logits, dim=1)
pred = torch.argmax(probs, dim=1).item()
print("  Prediction: classe %d (poids aleatoires pour la tete)" % pred)
print("\nOK - Le modele charge et fonctionne correctement!")
