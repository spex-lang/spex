from flask import Flask
from flask import jsonify
from flask import request

app = Flask(__name__)

# NOTE: Not thread-safe and won't work if clients connect concurrently.
petstore = {}

@app.route("/pet/<pet_id>", methods=["GET"])
def get_pet(pet_id):
    if pet_id in petstore:
        pet_name = petstore[pet_id]
        return jsonify(petId=pet_id, petName=pet_name)
    else:
        return "Pet not found", 404

@app.route("/pet", methods=["POST"])
def add_pet():
    pet_id = request.json.get("petId", None)
    pet_name = request.json.get("petName", None)
    if pet_id in petstore:
        return jsonify(error="Pet already exists"), 409
    petstore[pet_id] = pet_name
    return "[]", 201

@app.route("/health", methods=["GET"])
def health():
    return "[]", 200

@app.route("/_reset", methods=["DELETE"])
def _reset():
    petstore.clear()
    return "[]", 200

if __name__ == "__main__":
    app.run()
