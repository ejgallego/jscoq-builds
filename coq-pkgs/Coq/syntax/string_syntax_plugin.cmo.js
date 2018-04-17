function(T){"use strict";var
s="String",o=250,i=246,F="string",E="Strings",D="Coq",b=T.jsoo_runtime,a=b.caml_new_string,n=b.caml_obj_tag,C=b.caml_register_global,R=b.caml_wrap_exception;function
c(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
d(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
Q(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
S(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
g=b.caml_get_global_data(),t=a("string_syntax_plugin"),k=[0,a(D),[0,a(E),[0,a(s),0]]],h=g.DAst,m=g.CamlinternalLazy,y=g.Globnames,r=g.Buffer,l=g.Ascii_syntax_plugin,K=g.Char,H=g.Coqlib,P=g.Notation;c(g.Mltop[10],t);var
j=[248,a("String_syntax.Non_closed_string"),b.caml_fresh_oo_id(0)],u=d(l[1][5],k,a(F)),p=d(l[1][4],k,a(F)),v=[3,[0,[0,p,0],1]],w=[3,[0,[0,p,0],2]],J=a("EmptyString"),I=a(s),G=a("String interpretation"),N=[0,a(D),[0,a(E),[0,a(s),0]]],O=a("string_scope");function
q(a){return Q(H[1],G,k,a)}var
e=[i,function(a){return q(I)}],f=[i,function(a){return q(J)}];function
x(e,b){var
a=c(h[1],e);return 0===a[0]?d(y[5],a[1],b):0}function
z(a,j){var
r=b.caml_ml_string_length(j);function
k(g){if(g===r){var
p=n(f),s=0,t=o===p?f[1]:i===p?c(m[2],f):f;return d(h[3],a,[0,t,s])}var
u=[0,k(g+1|0),0],v=b.caml_string_get(j,g),q=n(e),w=[0,d(l[1][14],a,v),u],x=0,y=o===q?e[1]:i===q?c(m[2],e):e,z=[4,d(h[3],a,[0,y,x]),w];return d(h[3],a,z)}return k(0)}function
A(a){var
b=a[1];try{var
k=c(r[1],16),g=function(u){var
p=u;for(;;){var
a=c(h[1],p);switch(a[0]){case
0:var
q=n(f),v=a[1],w=o===q?f[1]:i===q?c(m[2],f):f;if(d(y[5],v,w))return[0,c(r[2],k)];break;case
4:var
b=a[2];if(b){var
g=b[2];if(g)if(!g[2]){var
s=n(e),z=g[1],A=b[1],B=a[1],C=o===s?e[1]:i===s?c(m[2],e):e;if(x(B,C)){var
t=c(l[1][16],A);if(t){var
D=c(K[1],t[1]);d(r[10],k,D);var
p=z;continue}throw j}}}break}throw j}}(b);return g}catch(a){a=R(a);if(a===j)return 0;throw a}}var
L=[0,d(h[3],0,[0,v,0]),0],M=[0,[0,d(h[3],0,[0,w,0]),L],A,1];S(P[15],O,[0,u,N],z,M);var
B=[0,t,j,k,u,p,v,w,q,e,f,x,z,A];C(19,B,"String_syntax_plugin.String_syntax");C(20,[0,B],"String_syntax_plugin");return}
