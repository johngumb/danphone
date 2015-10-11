import os

def simulation():
    return os.getenv("RADIOSIM") == "true"
