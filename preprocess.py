import os

def using(file: str) -> str:
    with open(file) as fp:
        return fp.read()
    
def cfg_platform(platform: str, code: str) -> str:
    if os.name == platform:
        return code
    else:
        return ""