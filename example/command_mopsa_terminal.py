import subprocess

result = subprocess.run(
    ["mopsa-python", "test.py"],      # commande et arguments
    capture_output=True,
    text=True
)

# Récupérer stdout et stderr
print("STDOUT :")
print(result.stdout)
if "error" in result.stdout:
    print("Perdu")
elif "No alarm":
    print("gagné")
else:
    print("cas inconnu")

# print("STDERR :")
# print(result.stderr)
#
# # Vérifier le code de retour
# print("Code de sortie :", result.returncode)
