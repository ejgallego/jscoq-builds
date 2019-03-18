function(X){"use strict";var
K="int31",J="digits",I="Int31",e=X.jsoo_runtime,b=e.caml_new_string,H=e.caml_register_global,W=e.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):e.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):e.caml_call_gen(a,[b,c])}function
G(a,b,c,d){return a.length==3?a(b,c,d):e.caml_call_gen(a,[b,c,d])}var
c=e.caml_get_global_data(),p=b("int31_syntax_plugin"),k=[0,b("Coq"),[0,b("Numbers"),[0,b("Cyclic"),[0,b(I),[0,b(I),0]]]]],m=b("int31_scope"),t=c.Mltop,h=c.Bigint,g=c.DAst,u=c.Stdlib__list,f=c.Names,E=c.Notation,T=c.Pp,V=c.CErrors,L=c.Libnames;a(t[10],p);var
S=b("int31 are only non-negative numbers."),U=[0,b("interp_int31")],N=b(K),O=b(K),P=b(J),Q=b(J),R=b("Int31_syntax.Non_closed");function
i(b){var
c=d(u[19],f[1][6],b);return a(f[5][4],c)}function
v(c,b){var
e=a(f[1][6],b),g=i(c);return d(L[15],g,e)}function
j(e,c){var
b=a(g[1],e);return 0===b[0]?d(f[68][1],b[1],c):0}function
q(c,b){var
e=a(f[6][4],b);return d(f[23][3],c,e)}function
w(b,a){return q([0,i(b)],a)}function
M(d,c,b){var
e=a(f[6][4],c);return q([2,[0,i(d)],e],b)}var
x=v(k,N);function
l(a){return w(k,a)}var
n=[3,[0,[0,l(O),0],1]],r=[3,[0,[0,l(P),0],1]],s=[3,[0,[0,l(Q),0],2]],o=[248,R,e.caml_fresh_oo_id(0)];function
y(b,e){var
f=d(g[3],b,[0,n,0]),i=d(g[3],b,[0,r,0]),j=d(g[3],b,[0,s,0]);function
c(b,e){if(0<b){var
d=a(h[8],e),f=d[2],g=c(b-1|0,d[1]),k=f?j:i;return[0,k,g]}return 0}var
k=c(31,e),l=[4,f,a(u[9],k)];return d(g[3],b,l)}function
z(b){var
c=a(T[3],S);return G(V[6],b,U,c)}function
A(c,b){return a(h[20],b)?y(c,b):z(c)}function
B(k){var
d=a(g[1],k);if(4===d[0]){var
l=d[2];if(j(d[1],n)){var
c=l,b=h[5];for(;;){if(c){var
e=c[2],f=c[1];if(j(f,r)){var
c=e,b=a(h[11],b);continue}if(j(f,s)){var
i=a(h[11],b),c=e,b=a(h[9],i);continue}throw o}return b}}}throw o}function
C(a){var
b=a[1];try{var
c=[0,B(b)];return c}catch(a){a=W(a);if(a===o)return 0;throw a}}function
D(c,b){function
e(d){return a(c,b)}return d(t[14],e,p)}G(E[19],0,m,[0,A,C]);D(E[22],[0,0,m,[0,m],[0,x,k],[0,n,0],1]);var
F=[0,p,i,v,j,q,w,M,k,x,l,m,n,r,s,o,y,z,A,B,C,D];H(19,F,"Int31_syntax_plugin.Int31_syntax");H(20,[0,F],"Int31_syntax_plugin");return}
