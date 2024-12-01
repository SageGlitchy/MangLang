import manglang

while True:
    text=input('mang > ')
    result,error= manglang.run('<stdin>',text)
    
    if error:
        print(error.as_string())
    else:
        print(result)
    