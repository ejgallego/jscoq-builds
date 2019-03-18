function(bE){"use strict";var
ax="in",K="f",v=250,aw="lc",ar="[",j=246,q="quote",av="]",n="Quote",au="c",aq="plugins/quote/quote.ml",at="using",E=129,as="k",i=bE.jsoo_runtime,ap=i.caml_check_bound,c=i.caml_new_string,u=i.caml_obj_tag,Y=i.caml_register_global,W=i.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):i.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):i.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):i.caml_call_gen(a,[b,c,d])}function
X(a,b,c,d,e){return a.length==4?a(b,c,d,e):i.caml_call_gen(a,[b,c,d,e])}function
bD(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):i.caml_call_gen(a,[b,c,d,e,f])}var
e=i.caml_get_global_data(),S=c("quote_plugin"),d=e.EConstr,f=e.Util,k=e.Tacmach,h=e.Proofview,ak=e.Assert_failure,R=e.Tacticals,al=e.Tactics,aj=e.Constr_matching,p=e.Names,_=e.Pp,$=e.CErrors,Z=e.Coqlib,M=e.Termops,s=e.CamlinternalLazy,F=e.Constr,af=e.Option,ae=e.Patternops,ac=e.Stdlib,D=e.Ltac_plugin,am=e.CAst,an=e.Loc,l=e.Genarg,m=e.Stdarg,aY=[0,c(aq),497,15],aX=[0,c(aq),475,13],aV=c("invalid inversion scheme for quote."),aT=[0,c("Coq"),[0,c(q),[0,c(n),0]]],aO=c("M"),aN=c("Quote: not a simple fixpoint"),aL=c("End_idx"),aM=[0,c(n),0],aJ=c("Left_idx"),aK=[0,c(n),0],aH=c("Right_idx"),aI=[0,c(n),0],aF=c("varmap_find"),aG=[0,c(n),0],aD=c("Node_vm"),aE=[0,c(n),0],aB=c("Empty_vm"),aC=[0,c(n),0],ay=c(q),az=c(n),a0=c("cont"),a1=c("x"),a5=c(as),a6=c(at),a8=c(au),a9=c(ax),a_=c(av),ba=c(aw),bb=c(ar),bd=c(K),be=c(q),bh=c(as),bi=c(at),bk=c(au),bl=c(ax),bn=c(K),bo=c(q),br=[0,c(av),0],bs=c(aw),bt=c(ar),bv=c(K),bw=c(q),bz=c(K),bA=c(q),bC=c(q),aU=e.Not_found,aP=e.Global,aQ=e.Environ,aR=e.Reductionops,aA=e.UnivGen,aS=e.Stdlib__hashtbl,a2=e.Geninterp,aZ=e.Mltop;function
o(c,b){var
e=g(Z[2],az,[0,ay,c],b),f=a(aA[21],e);return a(d[8],f)}var
w=[j,function(a){return o(aC,aB)}],x=[j,function(a){return o(aE,aD)}],y=[j,function(a){return o(aG,aF)}],z=[j,function(a){return o(aI,aH)}],A=[j,function(a){return o(aK,aJ)}],B=[j,function(a){return o(aM,aL)}],G=a(f[20][1],[0,F[80]]);function
r(c){var
b=a(_[3],aN);return g($[6],0,0,b)}function
L(a,c){var
e=b(M[60],a,c);return b(d[3],a,e)}function
aa(c){var
b=a(p[1][8],c);return i.caml_int_of_string(g(f[15][4],b,1,i.caml_ml_string_length(b)-1|0))}function
ab(c){var
d=a(ac[22],c),e=b(ac[17],aO,d);return a(p[1][6],e)}function
H(k,j,i,c){var
e=b(d[3],k,j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
l=g[2],m=h[1],n=function(b){return a(d[11],c-b|0)},o=b(f[19][2],c,n),p=[0,a(d[28],[0,[0,[0,m,0],i+1|0],l]),o];return a(d[21],p)}}return r(0)}function
ad(j,c,e,m){function
k(n){var
h=n;for(;;){var
e=b(d[3],c,h);switch(e[0]){case
5:var
h=e[1];continue;case
9:var
l=e[2],i=e[1];if(b(d[44],c,i))if(b(d[66],c,i)===m){var
p=a(f[19][42],l);return[11,[0,ab(b(d[66],c,p))]]}var
q=b(f[19][15],k,l),r=g(d[5],0,c,i);return[4,g(ae[8],j,c,r),q];default:var
o=g(d[5],0,c,h);return g(ae[8],j,c,o)}}}return k(e)}function
N(J,I,q){var
t=a(h[67][4],q),c=a(k[42][4],q);try{var
$=b(d[75],c,J),i=$}catch(a){a=W(a);if(a!==F[54])throw a;var
i=r(0)}var
K=i[1],N=[0,K,b(d[2][2],c,i[2])],O=a(aP[2],0),P=b(aQ[62],O,N),w=L(c,a(d[8],P));if(14===w[0]){var
x=w[1],z=x[1];if(1===z[1].length-1)if(0===z[2]){var
l=x[2];if(1===l[1].length-1)if(1===l[2].length-1){var
A=l[3];if(1===A.length-1){var
B=b(d[84],c,A[1]),e=B[1],Q=B[2],R=a(f[17][1],e),S=g(aR[80],0,t,c),m=L(c,Q);if(13===m[0]){var
C=m[2],n=[0,0],o=[0,0],p=[0,0],T=m[4],U=function(i,z){var
m=b(d[84],c,z),g=m[2],h=a(f[17][1],m[1]);if(b(d[44],c,g))if(1===b(d[66],c,g)){p[1]=[0,H(c,a(f[17][5],e)[2],i,h)];return 0}var
q=b(d[83],c,g),r=q[2],A=q[1];if(r){var
w=r[2];if(w){var
k=w[2];if(k){var
l=k[2];if(l)if(!l[2]){var
D=l[1];if(b(d[44],c,k[1]))if(b(d[44],c,D)){var
x=u(y),E=v===x?y[1]:j===x?a(s[2],y):y;if(b(S,A,E)){o[1]=[0,H(c,a(f[17][5],e)[2],i,h)];return 0}}}}}}var
B=n[1],C=ad(t,c,g,(R+h|0)+1|0);n[1]=[0,[0,H(c,a(f[17][5],e)[2],i,h),C],B];return 0};b(f[19][14],U,T);var
D=a(af[3],p[1]),V=D?a(af[3],o[1]):D;if(V)r(0);var
E=b(d[3],c,C),X=7===E[0]?a(M[47],E[3]):C,Y=p[1],Z=g(f[17][16],G[4],I,G[1]),_=o[1];return[0,a(f[17][9],n[1]),_,X,Z,Y]}return r(0)}}}}return r(0)}function
I(g,e,k){var
h=k;for(;;){var
l=a(d[E][1],h),i=b(G[3],l,e);if(i)return i;var
c=b(d[3],g,h);switch(c[0]){case
5:var
h=c[1];continue;case
9:var
m=c[2],j=I(g,e,c[1]);if(j){var
n=function(a){return I(g,e,a)};return b(f[19][34],n,m)}return j;default:return 0}}}function
ag(e,c){var
h=e.length-1,b=u(x),l=h>>>1|0,i=v===b?x[1]:j===b?a(s[2],x):x,k=u(w),m=[0,c],n=v===k?w[1]:j===k?a(s[2],w):w,f=a(d[21],[0,n,m]);function
g(b){if(h<b)return f;if(l<b){var
j=b-1|0,m=[0,i,[0,c,ap(e,j)[j+1],f,f]];return a(d[21],m)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,ap(e,k)[k+1],o,n]];return a(d[21],p)}return g(1)}function
ah(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=u(B),h=v===c?B[1]:j===c?a(s[2],B):B,i=b(e),k=a(f[17][9],i);function
l(g,f){var
h=[0,f];if(g)var
b=u(z),i=v===b?z[1]:j===b?a(s[2],z):z,c=i;else
var
e=u(A),k=v===e?A[1]:j===e?a(s[2],A):A,c=k;return a(d[21],[0,c,h])}return g(f[17][16],l,k,h)}function
O(c,l,i){var
e=l;for(;;){var
j=g(k[35],c,e,i);if(j)return j;var
m=a(k[2],c),h=b(d[3],m,e);switch(h[0]){case
5:var
e=h[1];continue;case
9:var
n=h[2],o=function(a){return O(c,a,i)};return b(f[19][32],o,n);default:return 0}}}function
ai(e,b){var
i=a(k[2],e);function
f(a,b){if(b){var
h=b[2],c=b[1];return g(d[95],i,a,c)?b:O(e,a,c)?[0,a,[0,c,h]]:[0,c,f(a,h)]}return[0,a,0]}if(b){var
c=b[1];return f(c,ai(e,b[2]))}return 0}var
J=a(aS[25],[0,F[74],F[93]]);function
C(e,c){function
g(b){var
c=b[1];return[0,c,a(d[E][1],b[2])]}var
h=b(f[17][69],g,e),i=a(d[E][1],c),j=b(M[45],h,i);return a(d[8],j)}function
P(t,j,e,c){a(Z[3],aT);var
k=a(J[1],17),i=[0,0],l=[0,1];function
m(c){function
h(u){var
h=u;for(;;){if(h){var
n=h[1],v=h[2],w=n[2],x=n[1];try{var
y=X(aj[3],t,j,w,c),z=a(p[1][11][17],y),A=function(a){var
b=a[1],c=m(a[2]);return[0,aa(b),c]},B=C(b(f[17][69],A,z),x);return B}catch(a){a=W(a);if(a===aj[1]){var
h=v;continue}throw a}}var
o=e[2];if(o){var
q=e[5],D=o[1];if(q){var
F=q[1];if(I(j,e[4],c))return C([0,[0,1,c],0],F)}try{var
H=a(d[E][1],c),K=b(J[7],k,H);return K}catch(b){b=W(b);if(b===aU){var
r=C([0,[0,1,ah(l[1])],0],D);l[1]++;i[1]=[0,c,i[1]];var
G=a(d[E][1],c);g(J[5],k,G,r);return r}throw b}}var
s=e[5];if(s)return C([0,[0,1,c],0],s[1]);var
L=a(_[3],aV);return g($[3],0,0,L)}}return h(e[1])}var
h=b(f[17][69],m,c),n=e[3],o=a(f[17][9],i[1]);return[0,h,ag(a(f[19][12],o),n)]}function
Q(c){function
d(c,e){if(c){var
g=c[2],i=c[1],j=function(a){return d(g,[0,a,e])},k=a(R[66][61],i);return b(h[72][1],k,j)}var
l=a(f[17][9],e);return a(h[16],l)}return d(c,0)}function
aW(i,e){function
c(c){var
j=b(k[42][2],i,c);function
l(a){return b(k[42][2],a,c)}var
m=b(f[17][69],l,e);function
n(e){function
c(o){function
c(c){var
p=a(h[67][4],c),j=a(k[42][4],c),q=b(d[5],0,j),l=N(e,b(f[17][69],q,o),c),m=P(p,j,l,[0,a(h[67][2],c),0]),i=m[1];if(i)if(!i[2]){var
n=i[1],r=m[2];if(l[2]){var
s=a(d[21],[0,e,[0,r,n]]);return g(al[3],0,s,2)}var
t=a(d[21],[0,e,[0,n]]);return g(al[3],0,t,2)}throw[0,ak,aX]}return a(h[67][8],c)}var
i=Q(m);return b(h[72][1],i,c)}var
o=a(R[66][61],j);return b(h[72][1],o,n)}return a(h[67][9],c)}var
t=[0,o,w,x,y,z,A,B,G,r,L,aa,ab,H,ad,N,I,ag,ah,O,ai,J,C,P,Q,aW,function(i,o,g,e){function
c(c){var
j=b(k[42][2],g,c);function
l(a){return b(k[42][2],a,c)}var
m=b(f[17][69],l,e);function
n(c){function
e(p){function
e(e){var
q=a(h[67][4],e),j=a(k[42][4],e),r=b(d[5],0,j),l=N(c,b(f[17][69],r,p),e),m=P(q,j,l,[0,o,0]),g=m[1];if(g)if(!g[2]){var
n=g[1],s=m[2];return l[2]?a(i,a(d[21],[0,c,[0,s,n]])):a(i,a(d[21],[0,c,[0,n]]))}throw[0,ak,aY]}return a(h[67][8],e)}var
g=Q(m);return b(h[72][1],g,e)}var
p=a(R[66][61],j);return b(h[72][1],p,n)}return a(h[67][9],c)}];Y(70,t,"Quote_plugin.Quote");a(aZ[10],S);var
T=a(p[1][6],a0),U=a(p[1][6],a1);function
V(d,c){var
e=a(D[13][2][1],c),f=[0,[2,[1,b(am[1],0,U)]],0],h=[0,[1,b(am[1],0,T)],f],i=[3,b(an[11],0,h)],j=a2[5][1],k=b(p[1][11][5],U,e),l=[0,g(p[1][11][4],T,d,k),j],m=[29,b(an[11],0,i)];return b(D[13][23],l,m)}var
a3=0;function
a4(d,c,b,a,f){function
e(b){return V(a,b)}return X(t[26],e,b,d,c)}var
a7=[0,a6,[1,[5,a(l[16],D[2][8])],a5,0]],a$=[0,a_,[0,a9,[1,[5,a(l[16],m[11])],a8,a7]]],bc=[0,bb,[1,[0,[5,a(l[16],m[7])]],ba,a$]],bf=[0,[0,[0,be,[1,[5,a(l[16],m[7])],bd,bc]],a4],a3];function
bg(c,b,a,f){var
d=0;function
e(b){return V(a,b)}return X(t[26],e,b,c,d)}var
bj=[0,bi,[1,[5,a(l[16],D[2][8])],bh,0]],bm=[0,bl,[1,[5,a(l[16],m[11])],bk,bj]],bp=[0,[0,[0,bo,[1,[5,a(l[16],m[7])],bn,bm]],bg],bf];function
bq(c,a,d){return b(t[25],c,a)}var
bu=[0,bt,[1,[0,[5,a(l[16],m[7])]],bs,br]],bx=[0,[0,[0,bw,[1,[5,a(l[16],m[7])],bv,bu]],bq],bp];function
by(a,c){return b(t[25],a,0)}var
bB=[0,[0,[0,bA,[1,[5,a(l[16],m[7])],bz,0]],by],bx];bD(D[10][8],S,bC,0,0,bB);var
ao=[0,S,T,U,V];Y(78,ao,"Quote_plugin.G_quote");Y(79,[0,t,ao],"Quote_plugin");return}
