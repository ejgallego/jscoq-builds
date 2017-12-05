(function(X){"use strict";var
G="int31",F="digits",E="Int31",e=X.jsoo_runtime,a=e.caml_new_string,D=e.caml_register_global,V=e.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):e.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):e.caml_call_gen(a,[b,c])}function
U(a,b,c,d){return a.length==3?a(b,c,d):e.caml_call_gen(a,[b,c,d])}function
W(a,b,c,d,f){return a.length==4?a(b,c,d,f):e.caml_call_gen(a,[b,c,d,f])}var
d=e.caml_get_global_data(),r=a("int31_syntax_plugin"),j=[0,a("Coq"),[0,a("Numbers"),[0,a("Cyclic"),[0,a(E),[0,a(E),0]]]]],w=a("int31_scope"),q=d.Globnames,g=d.Bigint,h=d.CAst,s=d.List,f=d.Names,P=d.Pp,R=d.CErrors,H=d.Libnames,T=d.Notation;b(d.Mltop[12],r);var
O=a("int31 are only non-negative numbers."),Q=[0,a("interp_int31")],J=a(G),K=a(G),L=a(F),M=a(F),N=a("Int31_syntax.Non_closed");function
i(a){var
d=c(s[19],f[1][6],a);return b(f[5][4],d)}function
t(d,a){var
e=b(f[1][6],a),g=i(d);return c(H[17],g,e)}function
n(d,a){var
e=b(f[6][4],a);return c(f[23][3],d,e)}function
u(b,a){return n([0,i(b)],a)}function
I(d,c,a){var
e=b(f[6][4],c);return n([2,[0,i(d)],e],a)}var
v=t(j,J);function
k(a){return u(j,a)}var
l=[3,[0,[0,k(K),0],1]],o=[3,[0,[0,k(L),0],1]],p=[3,[0,[0,k(M),0],2]],m=[248,N,e.caml_fresh_oo_id(0)];function
x(a,e){var
f=c(h[1],a,[0,l,0]),i=c(h[1],a,[0,o,0]),j=c(h[1],a,[0,p,0]);function
d(a,e){if(0<a){var
c=b(g[8],e),f=c[2],h=d(a-1|0,c[1]),k=f?j:i;return[0,k,h]}return 0}var
k=d(31,e),m=[4,f,b(s[9],k)];return c(h[1],a,m)}function
y(a){var
c=b(P[3],O);return U(R[6],a,Q,c)}function
z(c,a){return b(g[20],a)?x(c,a):y(c)}function
A(n){var
e=n[1];if(4===e[0]){var
j=e[1][1];if(0===j[0]){var
r=e[2];if(c(q[5],j[1],l)){var
d=r,a=g[5];for(;;){if(d){var
f=d[1][1];if(0===f[0]){var
h=d[2],i=f[1];if(c(q[5],i,o)){var
d=h,a=b(g[11],a);continue}if(c(q[5],i,p)){var
k=b(g[11],a),d=h,a=b(g[9],k);continue}}throw m}return a}}}}throw m}function
B(a){try{var
b=[0,A(a)];return b}catch(a){a=V(a);if(a===m)return 0;throw a}}var
S=[0,[0,c(h[1],0,[0,l,0]),0],B,1];W(T[14],w,[0,v,j],z,S);var
C=[0,r,i,t,n,u,I,j,v,k,w,l,o,p,m,x,y,z,A,B];D(20,C,"Int31_syntax_plugin.Int31_syntax");D(21,[0,C],"Int31_syntax_plugin");return});
