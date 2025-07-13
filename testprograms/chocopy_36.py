# Mutating a nonlocal variable across multiple levels of nested functions
# using static chain dereferencing to resolve and update `a` in function A.

def A():
    a: int = 7
    
    def B():
        nonlocal a
        
        def C():
            nonlocal a
            
            def D():
                nonlocal a
                a = a + 3
                
            D()
            
        C()
        a = a * 100
        
    B()
    a = a // 2
    print(a)

A()
