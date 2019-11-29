        program simpson
        implicit none
        integer i,j,n
        real*8 h,a,b,suma1,suma2,f,total
        write(*,*) 'ingresar parametros a b n'
        read(*,*) a,b,n
        h=(b-a)/n
        suma1=0
        suma2=0
        total=0
        do i=2,n-1,2         !partida,termino, incremento
           write(*,*) 'do pares',i,a+h*i
           suma1=suma1+f(a+h*i)
        end do
        do i=1,n,2
           write(*,*) 'do impares',i,a+h*i
           suma2=suma2+f(a+h*i)
        end do
        total=(h/3)*(2*suma1+4*suma2+f(a)+f(b))
        write(*,*) 'el resultado de nuestra integracion es', total
        end program
        
        function f(x)
        implicit none
        real*8 f,x
        f=sin(x)
        return
        end function

        
