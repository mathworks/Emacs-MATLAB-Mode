% Copyright (C) 2023 Uwe Brauer (and others)

% This program is free software: you can redistribute it and/or modify it under the terms of the GNU
% General Public License as published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
% even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
% General Public License for more details.

% You should have received a copy of the GNU General Public License along with this program. If not,
% see <http://www.gnu.org/licenses/>.

function [teuler,yeuler,ev]=fill_prob(f,intv,y0,N,TOL,nmax) % Se implementa la función correspondiente a 
                                                                  % la iteración simple+dato inicial
                                                            % el valor del paso anterior,
    yeuler = y0;                                                 % Se toman los valores iniciales que se dan como argumento
    a = min(intv);                                               
    b = max(intv); 
    h = (a + b)/N;
    teuler = a;
    
    tk = a;
    yk = y0;
    
    ev = 0;                                                       %el número de evaluaciones al
                                                                  % inicio es cero, pues no ha
                                                                  % habido ninguna iteración
    
    faux = @(t, y, f, h, z) [y + h*f(t, z)];                      % se define la función auxiliar para la iteración simple
    
    
    for i = 1:N
        
        t = tk + h;                                               % se toma los valores del tiempo correspondientes al paso 
        
        y = yk;
        diff = 100;                                              % se toma un numero arbitrariamente grande para inicializar el error de la iteración simple
        k = 0;                                                    % se inicializa el conteo para la iteración simple 
        
        while diff > TOL && k <= nmax                             % por una parte se condiciona al
                                                                  % while con un valor de tolerancia
                                                                  % para el error diff, error de una
                                                                  % iteración (de la iteración
                                                                  % simple), y, por otra parte, se
                                                                  % toma un numero máximo de
                                                                  % iteraciones porque puede
                                                                  % perfectamente el error de
                                                                  % iteración no ser menor que la
                                                                  % tolerancia, provocando un bucle
                                                                  % infinito
        
            k = k + 1;

            z = faux(t, yk, f, h, y);                              % hay que tener en cuenta que durante este bucle sólo se está considerando el tiempo t=tk +h con h valor de paso que corresponda, pero se mantiene fijo así como también se toma el valor inicial de y del paso anterior pero éste no está fijo 


            diff = max(max(abs(z - y)));                           % aquí se calcula el error de iteración, en este caso se utiliza la norma infinito
            y = z;                                                 % se da el nuevo valor para la siguiente iteración 
            
            ev = ev + 1;                                           % se hace el conteo del número de iteraciones 
        
        end

        yeuler = [yeuler, y];                                       % se actualizan los valores de 'y' y t para los vectores respuesta de la función
        teuler = [teuler, t];  
        
        tk = t;                                                     % se actualiza los valores de t
                                                                    % y 'y' para la siguiente
                                                                    % iteración del ciclo for
        yk = y;
        
    end
    

end

