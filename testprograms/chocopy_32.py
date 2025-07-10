# calling nested functions

class user_credentials(object):
    username: str = "default"
    password: str = "default"   

def foo(c: user_credentials):
    a: int = 0
    
    # nested bar fn
    def bar(flag: bool):
        
        def bar_inner():
            print("bar-inner")
            
        print("inner")
        bar_inner()
    
    print("foo called")
    bar(False)
    print(c.password)
    
# outer level bar function
def bar(bb: bool):
    print("outer")
    
creds: user_credentials = None
creds = user_credentials()

foo(creds)
bar(True)
