function(Z){"use strict";var
g=Z.jsoo_runtime,e=g.caml_new_string,j=g.caml_register_global,Y=g.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):g.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):g.caml_call_gen(a,[b,c])}function
x(a,b,c,d){return a.length==3?a(b,c,d):g.caml_call_gen(a,[b,c,d])}function
r(a,b,c,d,e){return a.length==4?a(b,c,d,e):g.caml_call_gen(a,[b,c,d,e])}var
b=g.caml_get_global_data(),o=e("string_notation_plugin"),l=b.Constrexpr_ops,y=b.Global,v=b.Smartlocate,z=b.Nametab,d=b.Pp,m=b.Libnames,A=b.CErrors,k=b.Names,t=b.Util,q=b.Vernacextend,p=b.Attributes,h=b.Stdarg,i=b.Genarg,am=b.CAst,ag=b.Evd,aq=b.Notation,B=b.Constrintern,w=b.Pretype_errors,s=b.Coqlib,F=b.Locality,C=b.Mltop;j(22,[0,0,0],"String_notation_plugin");var
al=[0,0],an=[0,0,1],au=[0,0,0],av=[0,1,1],aw=[0,1,0],ao=[0,0,1],ar=[0,0,0],as=[0,1,1],at=[0,1,0],X=e(" to Byte.byte or (option Byte.byte) or (list Byte.byte) or (option (list Byte.byte))."),_=e(" should go from "),J=e(")."),M=e(" or (option "),P=e(" should go from Byte.byte or (list Byte.byte) to "),I=e("core.byte.type"),H=e("core.list.type"),G=e("core.option.type"),L=e(":"),R=e("Notation"),S=e("String"),W=e("StringNotation");function
u(b){var
c=a(s[2],b);return x(z[45],0,k[1][10][1],c)}function
f(e,d,c,b){var
f=[0,a(l[9],c),[0,b]],g=a(l[10],f);try{r(B[10],e,d,0,g);var
h=1;return h}catch(a){a=Y(a);if(a[1]===w[1])return 0;throw a}}function
n(af,k,j,i,ae){var
b=a(y[2],0),e=a(ag[17],b);function
B(c,b){return a(l[14],[0,c,[0,b,0]])}function
q(b){return a(l[9],b)}var
n=q(u(I)),ah=q(u(H)),ai=q(u(G));function
r(a){return B(ai,a)}var
s=B(ah,n),w=a(v[4],k),aj=c(v[3],0,j),ak=c(v[3],0,i),g=q(k);function
h(d,b){var
e=[0,[0,c(am[1],0,0),0],al,d,b];return a(l[13],e)}var
C=a(y[40],w)[2][4];function
D(a,b){return[3,[0,w,c(t[4],a,1)]]}var
E=c(t[24][16],D,C),F=a(t[24][11],E);if(f(b,e,j,h(s,g)))var
o=an;else
if(f(b,e,j,h(s,r(g))))var
o=au;else
if(f(b,e,j,h(n,g)))var
o=av;else
if(f(b,e,j,h(n,r(g))))var
o=aw;else
var
K=a(d[3],J),L=a(m[26],k),N=a(d[3],M),O=a(m[26],k),Q=a(d[3],P),R=a(m[26],j),S=c(d[12],R,Q),T=c(d[12],S,O),U=c(d[12],T,N),V=c(d[12],U,L),W=c(d[12],V,K),o=x(A[5],0,0,W);if(f(b,e,i,h(g,s)))var
p=ao;else
if(f(b,e,i,h(g,r(s))))var
p=ar;else
if(f(b,e,i,h(g,n)))var
p=as;else
if(f(b,e,i,h(g,r(n))))var
p=at;else
var
Y=a(d[3],X),Z=a(m[26],k),$=a(d[3],_),aa=a(m[26],i),ab=c(d[12],aa,$),ac=c(d[12],ab,Z),ad=c(d[12],ac,Y),p=x(A[5],0,0,ad);var
ap=[0,af,ae,[2,[0,o,aj,p,ak,k,0]],[0,a(z[38],[2,w]),0],F,1];return a(aq[23],ap)}j(38,[0,n],"String_notation_plugin__String_notation");a(C[9],o);var
D=0,E=0;function
K(g,f,e,d,b){var
h=c(p[1],p[7],b);return[0,function(c){var
b=a(k[1][8],d);return n(a(F[7],h),g,f,e,b)}]}var
N=[0,L,[1,[5,a(i[16],h[7])],0]],O=[1,[5,a(i[16],h[17])],N],Q=[1,[5,a(i[16],h[17])],O],T=[0,[0,0,[0,S,[0,R,[1,[5,a(i[16],h[17])],Q]]],K,E],D],U=0,V=[0,function(a){return q[6]}];r(q[2],W,V,U,T);j(45,[0,o],"String_notation_plugin__G_string");return}
