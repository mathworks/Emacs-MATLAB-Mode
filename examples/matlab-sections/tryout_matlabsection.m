%% Preamble
clc
clear all

set(0,'defaultAxesTickLabelInterpreter','default');
set(0,'defaultTextInterpreter','latex');
set(0,'DefaultLegendInterpreter','latex');
set(0,'defaultAxesFontSize',13);

%% This is the first section, setting parameters

m = 1.0;
k = 4;
c = 2*0.01*sqrt(k*m);

F = 1.0;
Om = 4.0;

%% Second section - Transient Analysis
fsamp = 1024;
Tmax = 250*2*pi/Om;
Nt = fix(Tmax*fsamp);

[t,y] = ode45(@(t,y) [y(2); -c/m*y(2)-k/m*y(1)-F/m*cos(Om*t)], ...
              (0:Nt)*Tmax/Nt, [0;0]);

fsz = 14;
figure(1)
clf()
sp = stackedplot(t, y, 'DisplayLabels', {'y1','y2'});
xlabel('Time (s)')
set(gca, 'FontSize', fsz)
grid on;

%% Third section
N = 1000;
fsz = 14;
figure(2)
clf()
plot(eig(randn(N,N))/sqrt(N), '.'); hold on
plot(cos((0:100)*2*pi/100), sin((0:100)*2*pi/100), '-');
axis equal; grid on
set(gca, 'FontSize', fsz)

%% Things to try out:
% 1. Move point to the beginning of the buffer and try out the
%    function `matlab-sections-forward-section'.
%    This should take you to the next section (while highlighting). Do
%    it a couple more times to navigate.
% 2. Try out `matlab-sections-move-section-down' and this action will be reversed.
% 3. Go to the third section and try the function
%    `matlab-sections-move-section-up'.
%    This should move the contents of this section to occur before the
%    previous section.   
% 4. Then try out the function `matlab-sections-backward-section'.
% 5. Next (ensure you have matlab-shell running) try out the
%    function `matlab-sections-shell-run-section' to run the contents of
%    the section in the shell.
%    I rewrote this based on the matlab-sections environ to be
%    consistent although `matlab-shell-run-section` does something
%    similar.  

