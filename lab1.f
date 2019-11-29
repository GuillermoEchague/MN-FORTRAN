         program lab1
		    implicit none
		    integer i,j,n,k			
		    real x(533), y(533),AB(5,6),sol(5)
		    real sum1(9),sum2(5)
			real A(5,5),B(5,6),yn(5)
C Se generan los vectores datos x(i),y(i)			
         open (file='datos1.dat',unit=5)
	     do i=1,533
		    read(5,*) x(i),y(i)
		    end do
c Se debe asignar el valor cero a las variables iniciales
		    do j=2,9
			sum1(1)=1
		    sum1(j)=0
		    end do			
C Se genera los terminos de la matriz A, de la forma AX=B		   		   
		    do i=1,533
		    do j=1,9
		    sum1(j)=x(i)**(j-1)+sum1(j)
		    end do
		    end do
C Se Ordena el Vector A
            do j=1,5
		      do n=1,5
             A(j,n)=sum1(j+n-1)
			   end do
			   end do
c Se debe asignar el valor cero a las variables iniciales
c para el vector solucion B, de AX=B
		    do j=1,5
		    sum2(j)=0
		    end do
C Se genera el vector solucion B, de AX=B		 
			   do j=1,5
			   do i=1,533
			   sum2(j)=y(i)*x(i)**(j-1)+sum2(j)
			   end do
			   end do		
C Se desarrolla el metodo de solucion para el sistema AX=B, de nuestro 
C problema el cual sera a traves de Gauss
! Comienza la solucion del sistema de ecuaciones
      do i=1,5
      do j=1,6
       AB(i,j)=0 !valor inicial de la matriz concadenada AB	  
      if (j.EQ.6) then
         do n=1,533
         AB(i,j)= (x(n)**(i-1))*y(n)+AB(i,j)
         end do
      else
         do n=1,533
         AB(i,j)= (x(n)**(j+i-2)) + AB(i,j)
         end do
      end if
      end do
      end do
	      do i=1,5
          do j=1,6
          B(i,j)=AB(i,j)
         end do
         end do
         do k=1,5
          do i=1,5
          do j=1,6
             if (i.NE.k) then
             AB(i,j)= B(i,j) - (B(i,k) / B(k,k))* B(k,j)
             end if
          end do
          end do
           do i=1,5
             do j=1,6
             B(i,j)=AB(i,j)            
			   end do
             end do
       end do
      do n=1,6
      sol(n)=AB(n,6)/AB(n,n)
      end do  
		 	   write (*,*) 'Sistema AX=B'	   
			   write (*,*) 'Matriz A'
			   write (*,*) A
			   write (*,*) 'Vector solucion B'
               write (*,*) sum2
			   write (*,*) 'solucion del sistema'
			   write (*,*) sol				   
         End program 
		 
