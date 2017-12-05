(function(ac){"use strict";var
L=250,K="Ascii",p=246,J="ascii",b=ac.jsoo_runtime,H=b.caml_ml_string_length,c=b.caml_new_string,G=b.caml_obj_tag,I=b.caml_register_global,n=b.caml_string_get,aa=b.caml_wrap_exception;function
e(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
d(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
F(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
ab(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
a=b.caml_get_global_data(),q=c("ascii_syntax_plugin"),h=[0,c("Coq"),[0,c("Strings"),[0,c(K),0]]],g=a.Util,i=a.Coqlib,l=a.Globnames,z=a.CamlinternalLazy,m=a.CAst,k=a.Names,Y=a.Option,X=a.Pervasives,W=a.Printf,S=a.Pp,U=a.CErrors,M=a.Libnames,$=a.Notation;e(a.Mltop[12],q);var
j=[248,c("Ascii_syntax.Non_closed_ascii"),b.caml_fresh_oo_id(0)],V=[0,[4,0,[0,2,3],0,0],c("%03d")],R=c("Expects a single character or a three-digits ascii code."),T=[0,c("interp_ascii_string")],Q=c(K),P=c("Ascii interpretation"),N=c(J),O=c(J),_=c("char_scope");function
o(a){var
b=d(g[17][17],k[1][6],a);return e(k[5][4],b)}function
r(b,a){var
c=e(k[1][6],a),f=o(b);return d(l[25],f,c)}function
s(b,a){var
c=e(k[1][6],a),f=o(b);return d(M[17],f,c)}var
t=s(h,N),u=r(h,O),v=[0,[0,u,0],1],w=[3,v];function
x(a){return F(i[1],P,h,a)}var
f=[p,function(a){return x(Q)}];function
y(a,g){function
b(e,c){if(0===e)return 0;var
f=b(e-1|0,c/2|0),g=0,h=0===(c%2|0)?i[27]:i[26];return[0,d(m[1],a,[0,h,g]),f]}var
c=G(f),h=b(8,g),j=0,k=L===c?f[1]:p===c?e(z[2],f):f,l=[4,d(m[1],a,[0,k,j]),h];return d(m[1],a,l)}function
A(f,a){if(1===H(a))var
d=n(a,0);else{if(3===H(a)){var
h=n(a,0);if(e(g[11],h)){var
i=n(a,1);if(e(g[11],i)){var
j=n(a,2);if(e(g[11],j))var
d=b.caml_int_of_string(a),c=1;else
var
c=0}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
k=e(S[3],R),d=F(U[6],f,T,k)}return y(f,d)}function
B(b){function
a(c,b){if(b){var
e=b[1][1];if(0===e[0]){var
f=b[2],g=e[1];if(d(l[5],g,i[26]))return 1+(2*a(c-1|0,f)|0)|0;if(d(l[5],g,i[27]))return 2*a(c-1|0,f)|0}}else
if(0===c)return 0;throw j}try{var
c=[0,function(h){var
b=h[1];if(4===b[0]){var
c=b[1][1];if(0===c[0]){var
g=G(f),i=b[2],k=c[1],m=L===g?f[1]:p===g?e(z[2],f):f;if(d(l[5],k,m))return a(8,i)}}throw j}(b)];return c}catch(a){a=aa(a);if(a===j)return 0;throw a}}function
C(a){if(32<=a)if(!(126<a)){var
b=e(X[17],a);return d(g[15][1],1,b)}return d(W[4],V,a)}function
D(a){var
b=B(a);return d(Y[15],C,b)}var
Z=[0,[0,d(m[1],0,[0,w,0]),0],D,1];ab($[15],_,[0,t,h],A,Z);var
E=[0,q,j,o,r,s,h,t,u,v,w,x,f,y,A,B,C,D];I(25,E,"Ascii_syntax_plugin.Ascii_syntax");I(26,[0,E],"Ascii_syntax_plugin");return});
