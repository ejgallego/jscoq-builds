(function(S){"use strict";var
s="String",o=250,h=246,E="string",D="Strings",C="Coq",b=S.jsoo_runtime,a=b.caml_new_string,n=b.caml_obj_tag,B=b.caml_register_global,Q=b.caml_wrap_exception;function
g(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
c(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
P(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
R(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
f=b.caml_get_global_data(),t=a("string_syntax_plugin"),k=[0,a(C),[0,a(D),[0,a(s),0]]],m=f.CamlinternalLazy,z=f.Globnames,r=f.Buffer,l=f.Ascii_syntax_plugin,i=f.CAst,J=f.Char,G=f.Coqlib,O=f.Notation;g(f.Mltop[12],t);var
j=[248,a("String_syntax.Non_closed_string"),b.caml_fresh_oo_id(0)],u=c(l[1][5],k,a(E)),p=c(l[1][4],k,a(E)),v=[3,[0,[0,p,0],1]],w=[3,[0,[0,p,0],2]],I=a("EmptyString"),H=a(s),F=a("String interpretation"),M=[0,a(C),[0,a(D),[0,a(s),0]]],N=a("string_scope");function
q(a){return P(G[1],F,k,a)}var
d=[h,function(a){return q(H)}],e=[h,function(a){return q(I)}];function
x(a,j){var
r=b.caml_ml_string_length(j);function
k(f){if(f===r){var
p=n(e),s=0,t=o===p?e[1]:h===p?g(m[2],e):e;return c(i[1],a,[0,t,s])}var
u=[0,k(f+1|0),0],v=b.caml_string_get(j,f),q=n(d),w=[0,c(l[1][13],a,v),u],x=0,y=o===q?d[1]:h===q?g(m[2],d):d,z=[4,c(i[1],a,[0,y,x]),w];return c(i[1],a,z)}return k(0)}function
y(a){try{var
i=g(r[1],16),b=function(u){var
k=u;for(;;){var
a=k[1];switch(a[0]){case
0:var
p=n(e),v=a[1],w=o===p?e[1]:h===p?g(m[2],e):e;if(c(z[5],v,w))return[0,g(r[2],i)];break;case
4:var
q=a[1][1];if(0===q[0]){var
b=a[2];if(b){var
f=b[2];if(f)if(!f[2]){var
s=n(d),x=f[1],y=b[1],A=q[1],B=o===s?d[1]:h===s?g(m[2],d):d;if(c(z[5],A,B)){var
t=g(l[1][15],y);if(t){var
C=g(J[1],t[1]);c(r[10],i,C);var
k=x;continue}throw j}}}}break}throw j}}(a);return b}catch(a){a=Q(a);if(a===j)return 0;throw a}}var
K=[0,c(i[1],0,[0,v,0]),0],L=[0,[0,c(i[1],0,[0,w,0]),K],y,1];R(O[15],N,[0,u,M],x,L);var
A=[0,t,j,k,u,p,v,w,q,d,e,x,y];B(19,A,"String_syntax_plugin.String_syntax");B(20,[0,A],"String_syntax_plugin");return});
