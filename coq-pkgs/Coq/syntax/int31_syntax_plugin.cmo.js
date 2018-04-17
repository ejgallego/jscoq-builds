function(Y){"use strict";var
G="int31",F="digits",E="Int31",d=Y.jsoo_runtime,a=d.caml_new_string,D=d.caml_register_global,W=d.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):d.caml_call_gen(a,[b])}function
e(a,b,c){return a.length==2?a(b,c):d.caml_call_gen(a,[b,c])}function
V(a,b,c,e){return a.length==3?a(b,c,e):d.caml_call_gen(a,[b,c,e])}function
X(a,b,c,e,f){return a.length==4?a(b,c,e,f):d.caml_call_gen(a,[b,c,e,f])}var
c=d.caml_get_global_data(),r=a("int31_syntax_plugin"),k=[0,a("Coq"),[0,a("Numbers"),[0,a("Cyclic"),[0,a(E),[0,a(E),0]]]]],w=a("int31_scope"),h=c.Bigint,f=c.DAst,s=c.List,g=c.Names,Q=c.Pp,S=c.CErrors,I=c.Globnames,H=c.Libnames,U=c.Notation;b(c.Mltop[10],r);var
P=a("int31 are only non-negative numbers."),R=[0,a("interp_int31")],K=a(G),L=a(G),M=a(F),N=a(F),O=a("Int31_syntax.Non_closed");function
i(a){var
c=e(s[19],g[1][6],a);return b(g[5][4],c)}function
t(c,a){var
d=b(g[1][6],a),f=i(c);return e(H[17],f,d)}function
j(d,c){var
a=b(f[1],d);return 0===a[0]?e(I[5],a[1],c):0}function
o(c,a){var
d=b(g[6][4],a);return e(g[23][3],c,d)}function
u(b,a){return o([0,i(b)],a)}function
J(d,c,a){var
e=b(g[6][4],c);return o([2,[0,i(d)],e],a)}var
v=t(k,K);function
l(a){return u(k,a)}var
m=[3,[0,[0,l(L),0],1]],p=[3,[0,[0,l(M),0],1]],q=[3,[0,[0,l(N),0],2]],n=[248,O,d.caml_fresh_oo_id(0)];function
x(a,d){var
g=e(f[3],a,[0,m,0]),i=e(f[3],a,[0,p,0]),j=e(f[3],a,[0,q,0]);function
c(a,e){if(0<a){var
d=b(h[8],e),f=d[2],g=c(a-1|0,d[1]),k=f?j:i;return[0,k,g]}return 0}var
k=c(31,d),l=[4,g,b(s[9],k)];return e(f[3],a,l)}function
y(a){var
c=b(Q[3],P);return V(S[6],a,R,c)}function
z(c,a){return b(h[20],a)?x(c,a):y(c)}function
A(k){var
d=b(f[1],k);if(4===d[0]){var
l=d[2];if(j(d[1],m)){var
c=l,a=h[5];for(;;){if(c){var
e=c[2],g=c[1];if(j(g,p)){var
c=e,a=b(h[11],a);continue}if(j(g,q)){var
i=b(h[11],a),c=e,a=b(h[9],i);continue}throw n}return a}}}throw n}function
B(a){var
b=a[1];try{var
c=[0,A(b)];return c}catch(a){a=W(a);if(a===n)return 0;throw a}}var
T=[0,[0,e(f[3],0,[0,m,0]),0],B,1];X(U[14],w,[0,v,k],z,T);var
C=[0,r,i,t,j,o,u,J,k,v,l,w,m,p,q,n,x,y,z,A,B];D(20,C,"Int31_syntax_plugin.Int31_syntax");D(21,[0,C],"Int31_syntax_plugin");return}
