function(dJ){"use strict";var
H=148,aI="Firstorder",bq="already done",br=105,bz=",",bp="gintuition",t=121,bs=119,am=142,by="with",an="firstorder",bx="ground_plugin",bw="Solver",aH="using",bv="-----",Z=144,bo=114,bu=248,bt="reversible in 1st order mode",F=100,o=dJ.jsoo_runtime,A=o.caml_check_bound,bn=o.caml_fresh_oo_id,g=o.caml_new_string,E=o.caml_register_global,p=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
q(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}function
al(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):o.caml_call_gen(a,[b,c,d,e,f])}function
dH(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):o.caml_call_gen(a,[b,c,d,e,f,g])}function
dI(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):o.caml_call_gen(a,[b,c,d,e,f,g,h,i])}var
f=o.caml_get_global_data(),aj=g(bx),bd=[0,g(bx),g("auto_with")],e=f.EConstr,c=f.Util,K=f.Int,x=f.Not_found,r=f.Stdlib__queue,L=f.Reductionops,u=f.Termops,$=f.Constr,v=f.Context,ad=f.Hipattern,N=f.Global,ab=f.Inductiveops,aq=f.CClosure,y=f.Names,ay=f.Evd,l=f.Pp,aX=f.Ppconstr,ag=f.Printer,az=f.Hints,S=f.Stdlib,T=f.CErrors,af=f.Typing,P=f.Option,at=f.Heap,m=f.Tacmach,h=f.Tactics,d=f.Tacticals,i=f.Proofview,U=f.Ltac_plugin__Tacinterp,bc=f.Feedback,ba=f.Stdlib__list,aE=f.Ltac_plugin__Pptactic,ak=f.Vernacextend,aD=f.Attributes,W=f.Ltac_plugin__Tacarg,w=f.Genarg,I=f.Stdarg,J=f.Pcoq,Y=f.CLexer,aF=f.Ltac_plugin__Tacentries,bQ=f.Constrextern,bV=f.Control,b6=f.Evarutil,b$=f.Classops,ca=f.TransparentState,cK=f.Pputils,cs=f.Ltac_plugin__Tacintern,ct=f.Locality,cj=f.Auto,cd=f.Mltop,ci=f.Goptions,cm=f.Ltac_plugin__Tacenv,cn=f.CAst,cp=f.Ltac_plugin__Tactic_option,cP=f.CWarnings,cX=f.Geninterp;E(55,[0,0,0,0,0,0,0,0],"Ground_plugin");var
B=[bu,g("Ground_plugin.Unify.UFAIL"),bn(0)],bC=g("Ground_plugin.Formula.Is_atom"),bE=[0,0,[0,0,0]],bF=g("_"),bR=g(" : "),bS=g("| "),bT=g(bv),bU=g(bv),bN=g(" : No such Hint database"),bO=g("Firstorder: "),bK=[0,0],bJ=[0,0],bI=[0,0],bZ=g(bt),bY=g("No link"),bX=g("No axiom link"),bW=g("Not the expected number of hyps."),b_=g("not implemented ... yet"),b8=g("Untypable instance, maybe higher-order non-prenex quantification"),b7=g(bq),b9=g(bq),b3=g("can't happen."),b4=g("x"),cb=[0,0],cc=g(bt),cM=g('Deprecated syntax; use "," as separator'),cE=g("Firstorder solver tactic is "),cf=[0,g(aI),[0,g("Depth"),0]],cg=g("Firstorder Depth"),co=g("Firstorder default solver"),cv=g(bw),cw=g(aI),cx=g("Set"),cB=g("Firstorder_Set_Solver"),cF=[0,g("Print"),[0,g(aI),[0,g(bw),0]]],cJ=g("Firstorder_Print_Solver"),cN=g("deprecated"),cO=g("firstorder-deprecated-syntax"),c2=g(aH),c5=g(bz),c8=g(bz),c$=g(aH),df=g(aH),di=g("firstorder_using"),dm=g(by),dq=g(an),dt=g(by),dv=g(an),dz=g(an),dB=g(an),dE=g(bp),dG=g(bp);function
aJ(a){return b(e[t][1],-1,a)}function
ao(f,d){function
g(b){var
c=b[1];return[0,c,a(e[am][1],b[2])]}var
h=b(c[22][68],g,f),i=a(e[am][1],d),j=b(u[41],h,i);return a(e[9],j)}function
ap(g,W,V){var
i=a(r[2],0),q=[0,0];function
v(e,d){var
f=a(c[3],q);function
g(a){var
b=a[1];return[0,b,ao([0,[0,e,d],0],a[2])]}q[1]=[0,[0,e,d],b(c[22][68],g,f)];return 0}function
w(d){var
f=b(e[3],g,d);if(2===f[0]){var
h=f[1];try{var
i=a(c[3],q),j=w(b(K[4][2],h,i));return j}catch(a){a=p(a);if(a===x)return d;throw a}}return d}b(r[3],[0,W,V],i);try{for(;;){var
E=a(r[5],i),X=E[2],k=w(b(L[28],g,E[1])),l=w(b(L[28],g,X)),h=b(e[3],g,k),d=b(e[3],g,l);switch(h[0]){case
2:var
s=h[1];if(2===d[0]){var
C=d[1];if(1-(s===C?1:0))if(s<C)v(C,k);else
v(s,l)}else{var
z=ao(a(c[3],q),l),$=b(u[34],g,z),at=a(K[2][2],$)?j(u[29],g,s,z)?0:(v(s,z),1):0;if(!at)throw[0,B,k,l]}var
f=3;break;case
6:var
aa=h[3],ab=h[2];switch(d[0]){case
2:var
f=0;break;case
5:var
f=1;break;case
6:var
J=d[3],I=d[2],H=aa,G=ab,f=4;break;default:var
f=2}break;case
7:var
ae=h[3],af=h[2];switch(d[0]){case
2:var
f=0;break;case
5:var
f=1;break;case
7:var
J=d[3],I=d[2],H=ae,G=af,f=4;break;default:var
f=2}break;case
9:var
M=h[2],ag=h[1];switch(d[0]){case
2:var
f=0;break;case
5:var
f=1;break;case
9:var
N=d[2];b(r[3],[0,ag,d[1]],i);var
O=M.length-1;if(O!==N.length-1)throw[0,B,k,l];var
P=b(c[5],O,1),ah=0;if(!(P<0)){var
m=ah;for(;;){var
ai=A(N,m)[1+m],aj=[0,A(M,m)[1+m],ai];b(r[3],aj,i);var
ak=m+1|0;if(P!==m){var
m=ak;continue}break}}var
f=3;break;default:var
f=2}break;case
13:var
Q=h[4],al=h[3],am=h[2];switch(d[0]){case
2:var
f=0;break;case
5:var
f=1;break;case
13:var
R=d[4],an=d[3];b(r[3],[0,am,d[2]],i);b(r[3],[0,al,an],i);var
S=Q.length-1;if(S!==R.length-1)throw[0,B,k,l];var
T=b(c[5],S,1),ap=0;if(!(T<0)){var
n=ap;for(;;){var
aq=A(R,n)[1+n],ar=[0,A(Q,n)[1+n],aq];b(r[3],ar,i);var
as=n+1|0;if(T!==n){var
n=as;continue}break}}var
f=3;break;default:var
f=2}break;default:var
f=0}switch(f){case
0:if(2===d[0]){var
F=d[1],y=ao(a(c[3],q),k),_=b(u[34],g,y);if(a(K[2][2],_))if(j(u[29],g,F,y))var
D=1;else{v(F,y);var
o=2,D=0}else
var
D=1;if(D)throw[0,B,k,l]}else
if(5===h[0]){var
Z=[0,b(u[56],g,k),l];b(r[3],Z,i);var
o=2}else
var
o=0;break;case
1:var
o=0;break;case
2:var
o=1;break;case
3:var
o=2;break;default:b(r[3],[0,G,I],i);var
ac=aJ(J),ad=[0,aJ(H),ac];b(r[3],ad,i);var
o=3}switch(o){case
0:if(5===d[0]){var
Y=[0,k,b(u[56],g,l)];b(r[3],Y,i);var
t=1}else
var
t=0;break;case
1:var
t=0;break;case
2:var
t=1;break;default:var
t=2}switch(t){case
0:if(1-j(e[br],g,k,l))throw[0,B,k,l];var
U=0;break;case
1:var
U=0;break;default:var
U=1}continue}}catch(b){b=p(b);if(b===r[1])return a(c[3],q);throw b}}function
bA(a,h,d){function
f(d){if(b(e[56],a,d))if(b(e[75],a,d)===h)return 0;function
i(a,e){var
d=f(e);return 0<=a?0<=d?b(c[4],a,d):a:d}var
g=q(e[bs],a,i,-1,d);return 0<=g?b(c[4],g,1):-1}return f(d)}function
bB(d,h){var
f=[0,1],g=[0,0];function
i(h,j){var
k=b(e[3],d,j);if(2===k[0]){var
l=k[1];try{var
q=a(c[3],g),r=b(K[4][2],l,q),s=b(c[4],h,r),t=a(e[10],s);return t}catch(d){d=p(d);if(d===x){var
m=a(c[3],f);f[1]++;g[1]=[0,[0,l,m],a(c[3],g)];var
o=b(c[4],m,h);return a(e[10],o)}throw d}}function
n(a){return a+1|0}return al(e[112],d,n,i,h,j)}var
j=i(0,h),k=a(c[3],f);return[0,b(c[5],k,1),j]}function
aK(a,f,d,c,i){try{var
j=ap(a,c,i),g=b(K[4][2],f,j);if(b(e[56],a,g))var
h=[0,[1,d]];else
var
k=bA(a,f,c),h=[0,[0,bB(a,g),k]];return h}catch(a){a=p(a);if(a[1]===B)return 0;if(a===x)return[0,[1,d]];throw a}}function
aL(g,f,d){function
h(d){var
f=b(c[4],g,d);return a(e[12],f)}var
i=b(c[22][56],f,h);return b(e[t][4],i,d)}function
aM(g,f,d){var
a=f[1],h=d[2],i=d[1],j=aL(0,a,f[2]),k=aL(a,i,h);try{var
l=ap(g,j,k),m=function(c){var
d=c[1]<a?1:0,f=c[2];return d?d:b(e[56],g,f)},n=b(c[22][21],m,l);return n}catch(a){a=p(a);if(a[1]===B)return 0;throw a}}E(64,[0,B,ap,aK,aM],"Ground_plugin__Unify");var
s=[0,1],_=[0,aq[9]];function
ar(h,g,f,e,d,c){var
a=b(h,f,e);return 0===a?b(g,d,c):a}function
aN(j,i,h,g,f,e,d,c){var
a=q(j,h,g,f,e);return 0===a?b(i,d,c):a}var
M=[bu,bC,bn(0)];function
bD(a){return b(c[4],a,1)}function
aO(i,h){var
d=i,e=h;for(;;){var
f=a($[30],e);if(6===f[0]){var
g=f[3];if(0<d){var
d=b(c[5],d,1),e=g;continue}var
j=aO(0,g);return b(c[4],1,j)}return 0}}function
aa(e,d){var
f=a(N[40],d[1])[1][6],g=b(ab[4],e,d);function
h(a){return aO(f,a)}return b(c[24][15],h,g)}function
O(i,d,h,f,g){var
k=b(ab[4],i,f);function
l(c){var
i=a(e[9],c),k=a(N[40],f[1])[1][8],l=a(v[10][4],k),m=q(u[55],d,l,i,g),n=j(e[101],d,h,m)[2];return b(e[F],d,n)[1]}return b(c[24][15],l,k)}function
as(e,d,b){var
f=a(c[3],_);return q(L[16],f,e,d,b)}function
ac(g,d,C){var
B=a(c[3],_),f=q(L[17],B,g,d,C),o=j(ad[23],g,d,f);if(o){var
p=o[1],D=p[1];return[0,D,b(e[t][1],-1,p[2])]}var
r=j(ad[21],g,d,f);if(r){var
s=r[1];return[5,s[2],s[3]]}var
u=j(ad[27],g,d,f);if(u){var
i=u[1],k=i[2],E=i[3],v=b(e[85],d,i[1]),h=v[1],l=b(e[2][2],d,v[2]),w=a(N[40],h),m=w[2],x=m[4].length-1,F=w[1];if(0===x)return[1,[0,h,l],k];var
G=0<E?1:0,H=function(a){return 0===a?1:0},n=b(c[24][22],H,m[10]);if(!a(ab[21],[0,h,F,m])){var
K=G?n?1:0:1;if(K)return 1===x?[2,[0,h,l],k,n]:[3,[0,h,l],k,n]}return[6,f]}var
y=j(ad[29],g,d,f);if(y){var
z=y[1],I=z[2],A=b(e[85],d,z[1]),J=A[1];return[4,[0,J,b(e[2][2],d,A[2])],I]}return[6,as(g,d,f)]}var
C=[0,a(y[1][6],bF)];function
aP(m,h,p,g,d){var
n=[0,0],k=[0,0],l=[0,0];function
i(D,f,C){var
g=D,o=C;for(;;){var
d=ac(m,h,o);switch(d[0]){case
0:var
E=d[2];i(g,1-f,d[1]);var
o=E;continue;case
1:var
s=1-f,F=s?(n[1]=1,0):s;return F;case
4:var
x=d[2],L=d[1],M=a(p,1),N=a(e[12],M),P=A(O(m,h,1,L,x),0)[1],Q=function(d,j,c){var
h=a(v[10][1][4],c);return i([0,N,g],f,b(e[t][1],d,h))},R=a(c[22][1],x),S=b(c[5],2,R);return q(c[22][85],Q,S,0,P);case
5:var
T=d[2],U=a(p,1),g=[0,a(e[12],U),g],o=T;continue;case
6:var
r=j(e[t][3],g,0,d[1]),y=1-b(e[56],h,r);if(y){if(f){k[1]=[0,r,a(c[3],k)];return 0}l[1]=[0,r,a(c[3],l)];var
z=0}else
var
z=y;return z;default:var
G=d[2],H=d[1];if(d[3]){var
B=as(m,h,j(e[t][3],g,0,o));if(f)k[1]=[0,B,a(c[3],k)];else
l[1]=[0,B,a(c[3],l)]}var
u=O(m,h,0,H,G),I=function(d,j,c){var
h=a(v[10][1][4],c);return i(g,f,b(e[t][1],d,h))},J=function(d){var
e=a(c[22][1],d),f=b(c[5],1,e);return q(c[22][85],I,f,0,d)};if(f)var
K=function(a){return a?0:1},w=b(c[24][22],K,u);else
var
w=f;if(w)n[1]=1;return b(c[24][13],J,u)}}}switch(g){case
0:i(0,0,d);break;case
1:i(0,1,d);break;default:var
f=b(e[99],h,d),s=f[2],u=f[1],w=function(c){var
b=a(p,1);return a(e[12],b)};i(b(c[22][14],w,u),0,s);n[1]=0}var
o=a(c[3],l),r=[0,a(c[3],k),o];return[0,a(c[3],n),r]}function
aQ(h,g,o,w,f,n){function
k(a){return as(h,g,a)}try{var
q=bD(a(n,0)),r=a(c[3],s)?aP(h,g,n,o,f):bE,t=r[1],x=r[2];if(1===o){var
l=ac(h,g,f);switch(l[0]){case
0:var
i=0;break;case
1:var
i=3;break;case
2:var
i=1;break;case
3:var
i=2;break;case
4:var
z=A(O(h,g,0,l[1],l[2]),0)[1],B=a(c[22][br],z),i=[0,q,a(v[10][1][4],B),t];break;case
5:var
i=4;break;default:throw[0,M,l[1]]}var
u=[1,i]}else{var
d=ac(h,g,f);switch(d[0]){case
0:var
m=d[1],C=d[2],D=k(m),b=ac(h,g,m);switch(b[0]){case
0:var
e=[5,b[1],b[2],C];break;case
1:var
e=[0,b[1],b[2]];break;case
2:var
e=[1,b[1],b[2]];break;case
3:var
e=[2,b[1],b[2]];break;case
4:var
e=[4,b[1],b[2]];break;case
5:var
e=[3,m];break;default:var
e=0}var
j=[4,D,e];break;case
1:var
j=0;break;case
2:var
E=d[1];if(d[3])throw[0,M,k(f)];var
j=[0,E];break;case
3:var
F=d[1];if(d[3])throw[0,M,k(f)];var
j=[1,F];break;case
4:var
j=[3,d[1]];break;case
5:var
j=[2,q,d[1],t];break;default:throw[0,M,d[1]]}var
u=[0,j]}var
y=[0,[0,w,k(f),u,x]];return y}catch(a){a=p(a);if(a[1]===M)return[1,a[2]];throw a}}E(72,[0,s,_,ar,aN,aa,O,C,aP,aQ],"Ground_plugin__Formula");function
aR(a){if(0===a[0]){var
b=a[1];if(typeof
b==="number")return 999;else
switch(b[0]){case
0:return 90;case
1:return 40;case
2:return-30;case
3:return 60;default:var
c=b[2];if(typeof
c==="number")return 0;else
switch(c[0]){case
0:return F;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
0:return F;case
1:return 40;case
2:return-15;case
3:return-50;default:return F}return-29}var
bG=[0,function(d,a){var
e=aR(a[3]),f=aR(d[3]);return b(c[5],f,e)}],bH=[0,function(c,a){var
e=a[2],f=c[2],d=b(y[69][2][1],c[1],a[1]);if(0===d){var
g=function(c,a){var
d=o.caml_int_compare(c[1],a[1]),e=a[2],f=c[2];return 0===d?b($[87],f,e):d};return j(P[5],g,f,e)}return d}],k=a(c[26][1],[0,$[87]]),Q=a(c[25][1],bH);function
ae(g,a,f,c){var
d=j(e[5],bI,g,a);try{var
h=[0,f,b(k[23],d,c)],i=j(k[4],d,h,c);return i}catch(a){a=p(a);if(a===x)return j(k[4],d,[0,f,0],c);throw a}}function
aS(i,h,g,a){var
d=j(e[5],bJ,i,h);try{var
l=b(k[23],d,a),m=function(a){return 1-b(y[69][1],a,g)},f=b(c[22][61],m,l),n=f?j(k[4],d,f,a):b(k[6],d,a);return n}catch(b){b=p(b);if(b===x)return a;throw b}}var
D=a(at[2],bG);function
G(a){var
d=b(c[5],a[8],1);return[0,a[1],a[2],a[3],a[4],a[5],a[6],a[7],d]}function
au(c,a){var
d=a[8],e=b(Q[4],c,a[7]);return[0,a[1],a[2],a[3],a[4],a[5],a[6],e,d]}function
av(l,c,d){var
f=b(Q[3],c,d[7]);if(f)var
g=f;else{var
h=c[2],m=c[1];if(h){var
i=h[1],j=i[1],n=i[2],k=function(c){var
d=c[2],o=c[1];if(d){var
f=d[1],g=f[1],p=f[2],h=b(y[69][1],m,o);if(h){var
i=j<g?1:0;if(i){var
q=[0,j,a(e[9],n)];return aM(l,[0,g,a(e[9],p)],q)}var
k=i}else
var
k=h;return k}return 0};return b(Q[17],k,d[7])}var
g=0}return g}function
R(j,g,f,e,i,a){var
h=aQ(j,g,f,e,i,a[6]);if(0===h[0]){var
c=h[1];if(1===f){var
k=a[8],l=a[7],m=a[6],n=c[2],o=a[3],p=a[2];return[0,b(D[2],c,a[1]),p,o,n,0,m,l,k]}var
q=a[8],r=a[7],s=a[6],t=a[5],u=a[4],v=a[3],w=ae(g,c[2],e,a[2]);return[0,b(D[2],c,a[1]),w,v,u,t,s,r,q]}var
d=h[1];if(1===f)return[0,a[1],a[2],a[3],d,[0,d],a[6],a[7],a[8]];var
x=a[8],y=a[7],z=a[6],A=a[5],B=a[4],C=[0,d,a[3]],E=ae(g,d,e,a[2]);return[0,a[1],E,C,B,A,z,y,x]}function
aT(d,b,a){function
e(a,b){return a[1]===C?b:ae(d,a[2],a[1],b)}var
f=a[8],g=a[7],h=a[6],i=a[5],k=a[4],l=a[3],m=j(c[22][16],e,b,a[2]);return[0,j(c[22][16],D[2],b,a[1]),m,l,k,i,h,g,f]}function
aw(g,f,d){var
h=d[2],i=j(e[5],bK,g,f),l=b(k[23],i,h);return a(c[22][5],l)}function
ax(g,f){var
b=f;for(;;){var
c=a(D[3],b[1]),d=a(D[4],b[1]);if(c[1]===C){var
e=[0,d,b[2],b[3],b[4],b[5],b[6],b[7],b[8]];if(b[4]===c[2])return[0,c,e];var
b=e;continue}var
h=b[8],i=b[7],j=b[6],k=b[5],l=b[4],m=b[3];return[0,c,[0,d,aS(g,c[2],c[1],b[2]),m,l,k,j,i,h]]}}function
aU(f){var
b=[0,-1],g=Q[1];function
d(d){if(d)b[1]++;return a(c[3],b)}var
h=a(e[12],1);return[0,D[1],k[1],0,h,0,d,g,f]}function
bL(d){if(2===d[0]){var
e=d[1],f=function(a){return[3,[0,e,b(c[4],a,1)]]},g=a(N[2],0),h=b(ab[23],g,e);return b(c[22][56],h,f)}return[0,d,0]}var
bM=a(c[22][76],bL);function
aV(b,f,e,d){var
g=a(bM,e);function
h(c,a){var
g=a[1],d=dH(ay[173],0,0,0,b,a[2],c),e=q(af[2],0,b,d[1],d[2]),f=e[1];return[0,R(b,f,0,c,e[2],g),f]}return j(c[22][16],h,g,[0,d,f])}function
aW(f,d,h,g){var
e=[0,g];function
i(i){var
g=a(az[30],i[7]);switch(g[0]){case
0:case
2:case
3:var
h=g[1][1][1];try{var
k=b(u[99],d,h)[1],l=j(af[1],f,d,h);e[1]=R(f,d,2,k,l,a(c[3],e));var
m=0;return m}catch(a){a=p(a);if(a===x)return 0;throw a}default:return 0}}function
k(e,d,a){return b(c[22][11],i,a)}function
m(d){try{var
c=a(az[15],d),e=c}catch(c){c=p(c);if(c!==x)throw c;var
f=b(S[17],d,bN),g=b(S[17],bO,f),h=a(l[3],g),e=j(T[5],0,0,h)}return b(az[14][12],k,e)}b(c[22][11],m,h);return[0,a(c[3],e),d]}function
bP(c){function
d(h,g,f){var
c=a(N[2],0),d=a(ay[17],c),i=a(e[9],h),k=al(bQ[6],0,0,c,d,i),m=a(l[14],0),n=j(aX[17],c,d,k),o=a(l[3],bR),p=b(l[37],ag[39],g),q=a(l[3],bS),r=b(l[12],q,p),s=b(l[12],r,o),t=b(l[12],s,n),u=b(l[12],t,m);return b(l[12],u,f)}var
f=a(l[3],bT),g=a(l[7],0),h=j(k[12],d,c,g),i=a(l[14],0),m=a(l[3],bU),n=b(l[12],m,i),o=b(l[12],n,h),p=b(l[12],o,f);return b(l[24],0,p)}E(84,[0,[0,k[1],k[2],k[3],k[4],k[5],k[6],k[7],k[8],k[9],k[10],k[11],k[12],k[13],k[14],k[15],k[16],k[17],k[18],k[19],k[20],k[21],k[22],k[23],k[24],k[25],k[26]],Q,ae,aS,D,G,au,av,R,aT,aw,ax,aU,aV,aW,bP],"Ground_plugin__Sequent");function
n(n,k,h,r){function
d(d){a(bV[3],0);var
p=a(i[67][2],d),e=a(m[32][4],d),f=a(m[32][3],d);function
o(w,t,s){var
i=w,h=t,g=s;for(;;){if(0<i){if(h){var
p=h[2],k=h[1],n=a(v[11][1][2],k),x=a(m[32][5],d);if(!q(u[31],e,f,n,x)){var
y=j(u[32],e,f,n);if(!b(c[22][22],y,g)){var
z=o(b(c[5],i,1),p,[0,k,g]);return R(e,f,0,[0,n],a(v[11][1][4],k),z)}}var
i=b(c[5],i,1),h=p,g=[0,k,g];continue}var
A=a(l[3],bW);return j(T[2],0,0,A)}return r}}var
g=o(n,p,0),s=k?R(e,f,1,C,a(m[32][5],d),g):g;return a(h,s)}return a(i[67][7],d)}function
z(b){return 0===b[0]?a(h[76],[0,b[1],0]):d[57][2]}function
aY(e,c){function
f(f){try{var
j=function(b){return a(h[43],b)},k=aw(a(m[32][3],f),e,c),n=a(d[57][61],k),o=b(i[72][1],n,j);return o}catch(c){c=p(c);if(c===x){var
g=a(l[3],bX);return b(d[57][4],0,g)}throw c}}return a(i[67][7],f)}function
aZ(m,k,f,g,c){var
o=n(1,0,g,c),q=[0,h[16],0],r=[0,z(f),q];function
s(j){try{var
o=aw(j,m,c),q=a(i[16],o),g=q}catch(c){c=p(c);if(c!==x)throw c;var
k=a(l[3],bY),g=b(d[57][4],0,k)}function
n(c){function
g(c){function
g(b){var
d=[0,a(e[23],[0,b,[0,c]]),0];return a(h[H],d)}var
j=a(d[57][61],f);return b(i[72][1],j,g)}var
j=a(d[57][61],c);return b(i[72][1],j,g)}return b(i[72][1],g,n)}var
t=[0,b(i[72][1],i[54],s),r],u=a(d[57][22],t);return j(d[57][27],u,o,k)}function
a0(c,b,a){var
e=n(0,1,b,a);return j(d[57][27],h[120],e,c)}function
a1(f,e,c){var
g=n(0,1,e,c),i=[0,a(d[57][34],g)],j=b(h[110],0,i);return b(d[57][12],j,f)}function
a2(f,e,c){var
g=n(1,1,e,c),i=a(d[57][34],g),k=b(d[57][3],h[17],i),l=b(d[57][12],k,f),m=n(1,1,e,c);return j(d[57][27],h[16],m,l)}function
a3(l,k,c,g,f){function
e(o){var
e=A(aa(a(m[32][4],o),l),0)[1],p=n(e,0,g,f),q=[0,b(d[57][31],e,h[16]),0],r=[0,z(c),q],s=h[F],t=a(d[57][61],c),u=[0,b(i[72][1],t,s),r],v=a(d[57][22],u);return j(d[57][27],v,p,k)}return a(i[67][7],e)}function
a4(l,k,e,g,f){function
o(o){var
p=aa(a(m[32][4],o),l);function
q(c){var
i=[0,n(c,0,g,f),0],j=[0,b(d[57][31],c,h[16]),i],k=[0,z(e),j];return a(d[57][22],k)}var
r=b(c[24][15],q,p),s=h[F],t=a(d[57][61],e),u=b(i[72][1],t,s);return j(d[57][28],u,r,k)}return a(i[67][7],o)}function
a5(c){var
e=h[F],f=a(d[57][61],c);return b(i[72][1],f,e)}function
aA(f,l,s,k,r,q){var
u=f[2],v=f[1];function
g(o){var
w=a(m[32][3],o),p=O(a(m[32][4],o),w,0,f,l),g=p.length-1,x=a(c[24][12],l),y=n(g,0,r,q),B=[0,b(d[57][31],g,h[16]),0],C=[0,z(k),B];function
D(s){function
d(f){var
g=A(p,f)[1+f],d=a(c[22][1],g),h=a(e[2][1],u),i=[0,[0,v,b(c[4],f,1)],h],j=[0,a(e[30],i),x],k=a(e[23],j);function
l(f){var
g=b(c[5],d,f);return a(e[10],g)}var
m=b(c[24][2],d,l),n=[0,b(e[t][1],d,k),m],o=[0,a(e[23],n)],q=[0,b(e[t][1],d,s),o],r=a(e[23],q);return b(e[46],r,g)}var
f=b(c[22][56],g,d);return a(h[H],f)}var
E=a(d[57][61],k),F=[0,b(i[72][1],E,D),C],G=a(d[57][22],F);return j(d[57][27],G,y,s)}return a(i[67][7],g)}function
a6(k,j,m,l,c,g,f){var
o=b(e[t][1],1,j),p=[0,b(v[4],0,0),k,o],q=a(e[20],p),u=n(2,1,g,f),w=[0,a(d[57][34],u),0],x=[0,h[17],[0,h[17],w]],r=0,s=0,y=[0,z(c),x];function
A(m){var
c=a(e[10],2),d=b(e[t][1],1,k),f=[0,b(v[4],0,0),d,c],g=[0,m,[0,a(e[21],f)]],i=a(e[23],g),l=[0,b(v[4],0,0),j,i],n=[0,a(e[21],l),0];return a(h[H],n)}var
B=a(d[57][61],c),C=[0,b(i[72][1],B,A),y],D=[0,a(d[57][22],C),s];function
E(b){return a(h[43],b)}var
F=a(d[57][61],c),G=[0,b(i[72][1],F,E),D],I=a(h[Z],q),J=[0,b(d[57][21],I,G),r],K=[0,n(1,0,g,f),0],L=[0,z(c),K],M=[0,a(d[57][22],[0,h[17],L]),J],N=a(h[Z],m),O=b(d[57][21],N,M);return b(d[57][12],O,l)}function
a7(g,f,e){if(a(c[3],s))var
k=a(l[3],bZ),i=b(d[57][4],0,k);else
var
i=g;var
m=n(0,1,f,e),o=a(d[57][34],m),p=b(d[57][3],h[17],o),q=b(d[57][12],p,g),r=n(0,1,f,e),t=j(d[57][27],h[16],r,q);return b(d[57][12],t,i)}function
a8(o,l,e,k,g){function
f(p){var
f=A(aa(a(m[32][4],p),o),0)[1],q=[0,n(b(c[5],f,1),0,k,g),0],r=[0,b(d[57][31],f,h[16]),q],s=[0,z(e),r],t=a(d[57][22],s),u=h[F],v=a(d[57][61],e),w=b(i[72][1],v,u);return j(d[57][27],w,t,l)}return a(i[67][7],f)}function
a9(l,k,j,g,f){var
o=n(0,1,g,G(f)),p=[0,a(d[57][34],o),0],q=n(1,0,g,G(f)),r=[0,a(d[57][34],q),0],s=[0,h[16],r],t=[0,z(j),s];function
u(g){function
f(i){var
j=a(m[32][11],i),f=b(c[22][7],j,0),k=[0,g,[0,a(e[11],f)]],l=a(e[23],k),n=a(h[76],[0,f,0]),o=a(h[H],[0,l,0]);return b(d[57][3],o,n)}return a(i[67][7],f)}var
v=a(d[57][61],j),w=[0,b(i[72][1],v,u),t],x=[0,a(d[57][22],[0,h[16],w]),p],y=a(h[Z],l),A=b(d[57][21],y,x);return b(d[57][12],A,k)}E(90,[0,n,z,aY,aZ,a0,a1,a2,a3,a4,a5,aA,a6,a7,a8,a9],"Ground_plugin__Rules");function
b0(f,d){function
g(d,c){var
f=a(e[am][1],c),g=a(e[am][1],d);return b($[87],g,f)}if(0===f[0]){var
h=f[1],i=h[1],k=f[2],l=h[2];if(0===d[0]){var
j=d[1],m=d[2],n=j[2],o=j[1],p=c[5],q=c[5];return aN(function(a,b,c,d){return ar(q,p,a,b,c,d)},g,o,i,k,m,l,n)}return 0===i?1:-1}var
r=f[1];return 0===d[0]?0===d[1][1]?-1:1:g(r,d[1])}function
b1(c,a){return c===a?0:c===C?1:a===C?-1:b(y[69][2][1],c,a)}var
b2=[0,function(b,a){return ar(b0,b1,a[1],b[1],a[2],b[2])}],ah=a(c[25][1],b2);function
a_(F,d,m){var
e=[0,ah[1]];function
f(g){var
i=g[3];if(0===i[0]){var
d=i[1];if(typeof
d==="number")var
n=1;else
if(2===d[0])var
u=d[3],k=d[2],t=d[1],h=1,n=0;else
var
n=1;if(n)var
h=0}else{var
f=i[1];if(typeof
f==="number")var
h=0;else
var
u=f[3],k=f[2],t=f[1],h=1}if(h){var
v=g[4],p=[0,1],q=[0,u],C=g[1],r=function(f,d){function
g(h,g){var
d=aK(F,t,k,h,g);if(d){var
f=d[1];if(0===f[0]){p[1]=0;var
i=a(c[3],e);e[1]=b(ah[4],[0,f,C],i);return 0}q[1]=1;return 0}return 0}var
h=f[1];function
i(a){var
e=d[2];function
f(b){return g(a,b)}return b(c[22][11],f,e)}b(c[22][11],i,h);var
j=f[2];function
l(a){var
e=d[1];function
f(b){return g(a,b)}return b(c[22][11],f,e)}return b(c[22][11],l,j)},z=m[1],A=function(a){return r(v,a[4])};b(D[5],A,z);var
o=m[5],y=o?[0,o[1],0]:0;r(v,[0,y,m[3]]);var
s=a(c[3],p),w=s?a(c[3],q):s;if(w){var
E=a(c[3],e);e[1]=b(ah[4],[0,[1,k],g[1]],E);var
x=0}else
var
x=w;return x}var
B=a(l[3],b3);return j(T[2],0,0,B)}b(c[22][11],f,d);var
g=a(c[3],e);return a(ah[21],g)}function
ai(e,b){try{var
f=ax(e,b),g=f[1],a=g[3],k=f[2];if(0===a[0]){var
h=a[1];if(typeof
h==="number")var
d=1;else
if(2===h[0])var
c=1,d=0;else
var
d=1;if(d)var
c=0}else
var
c=typeof
a[1]==="number"?0:1;if(c)var
j=ai(e,k),i=[0,[0,g,j[1]],j[2]];else
var
i=[0,0,b];return i}catch(a){a=p(a);if(a===at[1])return[0,0,b];throw a}}var
a$=a(y[1][6],b4);function
b5(i,d,x,w,g,u){if(x===C)var
o=a$;else
var
H=j(af[1],i,d,w),I=j(L[29],i,d,H),s=b(e[79],d,I)[1][1],J=s?s[1]:a$,o=J;function
z(d){var
f=b(c[5],g,d);return a(e[10],f)}var
A=b(c[22][56],g,z),B=b(e[t][4],A,u),n=g,m=y[1][10][1],f=i,l=d,k=0;for(;;){if(0===n)return[0,l,k,B];var
p=j(h[13],m,o,f),q=dI(b6[7],0,0,0,0,0,f,l,ay[128]),D=q[2][1],E=q[1],r=[0,b(v[4],[0,p],0),D],F=b(e[122],r,f),G=b(y[1][10][4],p,m),n=b(c[5],n,1),m=G,f=F,l=E,k=[0,r,k];continue}}function
aB(g,f,r,k){function
o(o){var
s=a_(a(m[32][3],o),g,k);function
t(g){if(g[2]===C)var
f=g[1],o=function(j,k){function
g(B){if(0===f[0]){var
g=f[1];if(0===g[1]){var
o=g[2],p=[0,n(0,1,j,G(k)),0],q=a(d[57][35],p),r=a(h[bo],[0,[0,o,0]]);return b(d[57][3],r,q)}var
s=a(l[3],b_);return b(d[57][4],0,s)}var
t=f[1],u=[0,a(d[57][24],h[42]),0],v=[0,n(0,1,j,G(k)),0],w=[0,a(d[57][35],v),0];function
x(d){var
f=a(m[32][11],d),g=b(c[22][7],f,0),i=[0,[0,a(e[11],g),0]];return a(h[bo],i)}var
y=[0,a(i[67][7],x),w],z=[0,a(d[57][22],[0,h[17],y]),u],A=a(h[Z],t);return b(d[57][21],A,z)}return a(i[67][7],g)};else
var
o=function(w,k){var
f=g[2],o=g[1];function
r(x){var
g=a(m[32][3],x);if(0===o[0]){var
t=o[1],r=t[2],s=t[1],u=[0,s,j(e[5],0,g,r)];if(av(g,[0,f,[0,u]],k)){var
y=a(l[3],b7);return b(d[57][4],0,y)}if(0<s)var
z=function(g){function
c(c){var
o=a(m[32][3],c),d=b5(a(m[32][4],c),o,f,g,s,r),t=d[2],u=d[1],v=a(e[23],[0,g,[0,d[3]]]),k=b(e[46],v,t);try{var
A=a(m[32][4],c),B=q(af[2],0,A,u,k),n=B}catch(b){b=p(b);if(!a(T[13],b))throw b;var
w=a(l[3],b8),n=j(T[5],0,0,w)}var
x=n[1],y=a(h[H],[0,k,0]),z=a(i[65][1],x);return b(i[18],z,y)}return a(i[67][7],c)},A=a(d[57][61],f),v=b(i[72][1],A,z);else
var
D=function(b){var
c=[0,a(e[23],[0,b,[0,r]]),0];return a(h[H],c)},E=a(d[57][61],f),v=b(i[72][1],E,D);var
B=[0,n(1,0,w,G(au([0,f,[0,u]],k))),0],C=[0,a(d[57][35],B),0];return a(d[57][22],[0,v,[0,h[17],C]])}var
F=o[1];if(av(g,[0,f,0],k)){var
I=a(l[3],b9);return b(d[57][4],0,I)}var
J=[0,a(d[57][24],h[42]),0],K=[0,n(1,0,w,G(au([0,f,0],k))),0],L=[0,a(d[57][35],K),0],M=[0,h[17],L];function
N(d){function
f(f){var
g=a(m[32][11],f),i=b(c[22][7],g,0),j=[0,d,[0,a(e[11],i)]],k=[0,a(e[23],j),0];return a(h[H],k)}return a(i[67][7],f)}var
O=a(d[57][61],f),P=[0,b(i[72][1],O,N),M],Q=[0,a(d[57][22],[0,h[17],P]),J],R=a(h[Z],F);return b(d[57][21],R,Q)}return a(i[67][7],r)};return o(r,k)}var
u=b(c[22][68],t,s),v=a(d[57][26],u);return b(d[57][12],v,f)}return a(i[67][7],o)}E(92,[0,ai,a_,aB],"Ground_plugin__Instances");function
bb(T,g){function
c(h){function
c(a,d){var
c=d[1];if(1===c[0]){var
e=b(y[20][8],c[1],a[2]);return[0,a[1],e]}return a}var
e=a(b$[26],0),f=j(ba[20],c,ca[2],e);_[1]=b(aq[3][13],aq[9],f);function
v(x,k){function
c(r){if(o.caml_equal(a(U[8],0),cb)){var
V=a(m[32][3],r),W=[0,a(i[67][11],r),V],X=j(ag[65],0,0,W);b(bc[9],0,X)}try{var
D=ax(a(m[32][3],r),k),g=D[2],h=D[1],e=function(b){return aT(a(m[32][3],r),x,b)},Z=0,f=function(a){return v(Z,a)},c=v([0,h,x],g),y=h[3];if(0===y[0]){var
q=y[1];if(typeof
q==="number")var
t=a5(h[1]);else
switch(q[0]){case
0:var
_=q[1],$=e(g),t=a3(_,c,h[1],f,$);break;case
1:var
aa=q[1],ab=e(g),t=a4(aa,c,h[1],f,ab);break;case
2:var
F=ai(a(m[32][3],r),k),G=F[1],ac=F[2],H=v(b(S[26],G,x),ac);if(s[1])if(0<k[8])var
I=aB(G,H,f,e(k)),z=1;else
var
z=0;else
var
z=0;if(!z)var
I=H;var
t=I;break;case
3:var
ad=q[1];if(s[1])var
ae=e(g),J=a8(ad,c,h[1],f,ae);else
var
J=c;var
t=J;break;default:var
n=q[2],af=q[1];if(typeof
n==="number")var
w=c;else
switch(n[0]){case
3:var
am=n[1];if(0<k[8])if(s[1])var
an=e(g),K=a9(am,c,h[1],f,an),A=1;else
var
A=0;else
var
A=0;if(!A)var
K=c;var
w=K;break;case
4:var
ao=n[2],ap=n[1];if(s[1])var
aq=e(g),L=aA(ap,ao,c,h[1],f,aq);else
var
L=c;var
w=L;break;case
5:var
ar=n[3],as=n[2],au=n[1],av=e(g),w=a6(au,as,ar,c,h[1],f,av);break;default:var
aj=n[2],ak=n[1],al=e(g),w=aA(ak,aj,c,h[1],f,al)}var
ah=e(g),t=aZ(af,w,h[1],f,ah)}var
E=t}else{var
M=y[1];if(typeof
M==="number")switch(M){case
0:var
u=a2(c,f,e(g));break;case
1:var
u=a0(c,f,e(g));break;case
2:var
u=a1(c,f,e(g));break;case
3:var
u=c;break;default:if(s[1])var
aw=a(l[3],cc),N=b(d[57][4],0,aw);else
var
N=c;var
u=a7(N,f,e(g))}else{var
O=ai(a(m[32][3],r),k),P=O[1],ay=O[2],Q=v(b(S[26],P,x),ay);if(s[1])if(0<k[8])var
R=aB(P,Q,f,e(k)),B=1;else
var
B=0;else
var
B=0;if(!B)var
R=Q;var
u=R}var
E=u}var
C=E}catch(a){a=p(a);if(a!==at[1])throw a;var
C=T}var
Y=aY(k[4],k);return b(d[57][12],Y,C)}return a(i[67][7],c)}var
k=a(i[67][2],h),q=a(ba[1],k);return a(g,function(a){var
b=0;return n(q,1,function(a){return v(b,a)},a)})}return a(i[67][7],c)}E(98,[0,bb],"Ground_plugin__Ground");a(cd[9],aj);var
V=[0,3];function
ce(a){return a?(V[1]=b(S[6],a[1],0),0):(V[1]=3,0)}var
ch=[0,0,cg,cf,function(a){return[0,V[1]]},ce];b(ci[3],0,ch);var
ck=[0,bd,0],cl=[0,function(b,a){return q(cj[14],0,0,0,0)}];j(cm[16],0,bd,cl);var
be=[31,b(cn[1],0,[0,ck,0])],aC=b(cp[1],[0,be],co),bf=aC[3],bg=aC[2],bh=aC[1],cq=0,cr=0;function
cu(d,c){var
e=b(aD[1],aD[7],c);return[0,function(f){var
c=a(cs[2],d);return b(bh,a(ct[5],e),c)}]}var
cy=[0,[0,0,[0,cx,[0,cw,[0,cv,[1,[5,a(w[16],W[9])],0]]]],cu,cr],cq],cz=0,cA=[0,function(a){return ak[6]}];q(ak[2],cB,cA,cz,cy);var
cC=0,cD=0,cG=[0,[0,0,cF,function(c){a(aD[2],c);return[0,function(f){var
c=a(bf,0),d=a(l[3],cE),e=b(l[12],d,c);return b(bc[7],0,e)}]},cD],cC],cH=0,cI=[0,function(a){return ak[5]}];q(ak[2],cJ,cI,cH,cG);function
X(h,c,g,f){var
e=s[1];function
j(a){var
c=a[2],d=a[1];s[1]=e;return b(i[21],[0,c],d)}function
k(l){s[1]=h;var
j=c?c[1]:a(bg,0)[2],k=bb(j,function(h){function
c(c){var
j=aU(V[1]),k=a(m[32][3],c),l=aV(a(m[32][4],c),k,g,j)[1],n=a(m[32][3],c),e=aW(a(m[32][4],c),n,f,l),o=e[2],p=a(h,e[1]),q=a(i[65][1],o);return b(d[57][3],q,p)}return a(i[67][7],c)});s[1]=e;return k}var
l=a(i[67][7],k);return b(i[22],l,j)}function
bi(d,c,b){return a(aE[27],aX[7])}function
bj(f,e,d){function
b(b){return a(ag[39],b[2])}var
c=a(cK[5],b);return a(aE[27],c)}function
bk(d,c,b){return a(aE[27],ag[39])}function
cL(b){return a(l[22],cM)}var
bl=q(cP[1],cO,cN,0,cL);function
cQ(b,a){return bk}function
cR(b,a){return bj}var
cS=[0,function(b,a){return bi},cR,cQ],cT=[1,[1,I[17]]],cU=[1,[1,I[17]]],cV=[1,[1,I[17]]],cW=a(w[6],I[17]),cY=[0,[1,a(cX[3],cW)]],cZ=0;function
c0(a,c,b){return[0,a,0]}var
c1=[6,J[15][15]],c3=[0,[0,[0,[0,0,[0,a(Y[10],c2)]],c1],c0],cZ];function
c4(b,e,a,d,c){return[0,a,b]}var
c6=[0,a(Y[10],c5)],c7=[2,[6,J[15][15]],c6],c9=[0,a(Y[10],c8)],c_=[6,J[15][15]],da=[0,[0,[0,[0,[0,[0,0,[0,a(Y[10],c$)]],c_],c9],c7],c4],c3];function
db(d,c,a,f,e){b(bl,0,0);return[0,a,[0,c,d]]}var
dc=[3,[6,J[15][15]]],dd=[6,J[15][15]],de=[6,J[15][15]],dg=[0,[0,[0,[0,[0,[0,0,[0,a(Y[10],df)]],de],dd],dc],db],da],dh=[0,[1,[0,[0,0,function(a){return 0}],dg]],cY,cV,cU,cT,cS],bm=b(aF[9],di,dh),aG=bm[1],dj=bm[2],dk=0;function
dl(f,e,d,c){var
g=a(U[24],c);return X(1,b(P[16],g,f),e,d)}var
dn=[0,dm,[1,[0,[5,a(w[16],I[16])]],0]],dp=[1,[5,a(w[16],aG)],dn],dr=[0,[0,[0,dq,[1,[4,[5,a(w[16],W[9])]],dp]],dl],dk];function
ds(e,d,c){var
f=a(U[24],c);return X(1,b(P[16],f,e),0,d)}var
du=[0,dt,[1,[0,[5,a(w[16],I[16])]],0]],dw=[0,[0,[0,dv,[1,[4,[5,a(w[16],W[9])]],du]],ds],dr];function
dx(e,d,c){var
f=a(U[24],c);return X(1,b(P[16],f,e),d,0)}var
dy=[1,[5,a(w[16],aG)],0],dA=[0,[0,[0,dz,[1,[4,[5,a(w[16],W[9])]],dy]],dx],dw];al(aF[8],aj,dB,0,0,dA);var
dC=0;function
dD(d,c){var
e=a(U[24],c);return X(0,b(P[16],e,d),0,0)}var
dF=[0,[0,[0,dE,[1,[4,[5,a(w[16],W[9])]],0]],dD],dC];al(aF[8],aj,dG,0,0,dF);E(bs,[0,aj,V,be,bh,bg,bf,X,bi,bj,bk,bl,aG,dj],"Ground_plugin__G_ground");return}
