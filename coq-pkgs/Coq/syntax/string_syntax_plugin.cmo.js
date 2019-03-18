function(S){"use strict";var
u="String",o=250,h=246,K="string",J="Strings",I="Coq",c=S.jsoo_runtime,a=c.caml_new_string,n=c.caml_obj_tag,H=c.caml_register_global,R=c.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):c.caml_call_gen(a,[b])}function
d(a,b,d){return a.length==2?a(b,d):c.caml_call_gen(a,[b,d])}function
G(a,b,d,e){return a.length==3?a(b,d,e):c.caml_call_gen(a,[b,d,e])}var
g=c.caml_get_global_data(),p=a("string_syntax_plugin"),k=[0,a(I),[0,a(J),[0,a(u),0]]],t=a("string_scope"),v=g.Mltop,i=g.DAst,m=g.CamlinternalLazy,A=g.Names,s=g.Stdlib__buffer,l=g.Ascii_syntax_plugin,E=g.Notation,P=g.Stdlib__char,M=g.Coqlib;b(v[10],p);var
j=[248,a("String_syntax.Non_closed_string"),c.caml_fresh_oo_id(0)],w=d(l[1][5],k,a(K)),q=d(l[1][4],k,a(K)),x=[3,[0,[0,q,0],1]],y=[3,[0,[0,q,0],2]],O=a("EmptyString"),N=a(u),L=a("String interpretation"),Q=[0,a(I),[0,a(J),[0,a(u),0]]];function
r(a){return G(M[1],L,k,a)}var
e=[h,function(a){return r(N)}],f=[h,function(a){return r(O)}];function
z(e,c){var
a=b(i[1],e);return 0===a[0]?d(A[68][1],a[1],c):0}function
B(a,j){var
r=c.caml_ml_string_length(j);function
k(g){if(g===r){var
p=n(f),s=0,t=o===p?f[1]:h===p?b(m[2],f):f;return d(i[3],a,[0,t,s])}var
u=[0,k(g+1|0),0],v=c.caml_string_get(j,g),q=n(e),w=[0,d(l[1][14],a,v),u],x=0,y=o===q?e[1]:h===q?b(m[2],e):e,z=[4,d(i[3],a,[0,y,x]),w];return d(i[3],a,z)}return k(0)}function
C(a){var
c=a[1];try{var
k=b(s[1],16),g=function(u){var
p=u;for(;;){var
a=b(i[1],p);switch(a[0]){case
0:var
q=n(f),v=a[1],w=o===q?f[1]:h===q?b(m[2],f):f;if(d(A[68][1],v,w))return[0,b(s[2],k)];break;case
4:var
c=a[2];if(c){var
g=c[2];if(g)if(!g[2]){var
r=n(e),x=g[1],y=c[1],B=a[1],C=o===r?e[1]:h===r?b(m[2],e):e;if(z(B,C)){var
t=b(l[1][16],y);if(t){var
D=b(P[1],t[1]);d(s[10],k,D);var
p=x;continue}throw j}}}break}throw j}}(c);return g}catch(a){a=R(a);if(a===j)return 0;throw a}}function
D(c,a){function
e(d){return b(c,a)}return d(v[14],e,p)}G(E[20],0,t,[0,B,C]);D(E[22],[0,0,t,[0,t],[0,w,Q],[0,y,[0,x,0]],1]);var
F=[0,p,j,k,w,q,x,y,r,e,f,z,B,C,D];H(19,F,"String_syntax_plugin.String_syntax");H(20,[0,F],"String_syntax_plugin");return}
