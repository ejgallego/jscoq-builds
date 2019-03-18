function(ek){"use strict";var
aI="Firstorder",y=108,be="already done",aG="firstorder_using",bp=",",bh=250,bd="gintuition",aE=246,bo=119,aH="l",bg=113,bn="with",_="t",bc="Depth",an="firstorder",$=143,bm="ground_plugin",am=100,aF=109,bl="Solver",bk=248,aD="using",bj="-----",bf=103,M=146,bi="reversible in 1st order mode",al=129,t=ek.jsoo_runtime,F=t.caml_check_bound,ba=t.caml_fresh_oo_id,e=t.caml_new_string,bb=t.caml_obj_tag,J=t.caml_register_global,v=t.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):t.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):t.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):t.caml_call_gen(a,[b,c,d])}function
p(a,b,c,d,e){return a.length==4?a(b,c,d,e):t.caml_call_gen(a,[b,c,d,e])}function
z(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):t.caml_call_gen(a,[b,c,d,e,f])}function
L(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):t.caml_call_gen(a,[b,c,d,e,f,g])}function
ej(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):t.caml_call_gen(a,[b,c,d,e,f,g,h])}function
ei(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):t.caml_call_gen(a,[b,c,d,e,f,g,h,i])}var
f=t.caml_get_global_data(),ak=e(bm),aZ=[0,e(bm),e("auto_with")],N=f.Constr,j=f.Util,O=f.Context,d=f.EConstr,x=f.Termops,ae=f.Hipattern,aa=f.Global,ab=f.Inductiveops,S=f.Reductionops,ao=f.CClosure,A=f.Names,T=f.Int,C=f.Not_found,w=f.Stdlib__queue,av=f.Evd,l=f.Pp,ax=f.Ppconstr,ai=f.Printer,aw=f.Hints,P=f.Stdlib,W=f.CErrors,ah=f.Typing,aP=f.Globnames,U=f.Option,at=f.Heap,aS=f.CamlinternalLazy,h=f.Tactics,o=f.Tacmach,c=f.Tacticals,i=f.Proofview,r=f.Ltac_plugin,aX=f.Feedback,aW=f.Stdlib__list,m=f.Genarg,I=f.Stdarg,a4=f.Vernac_classifier,aY=f.Goptions,a5=f.Vernacentries,a_=f.Genintern,aB=f.Geninterp,E=f.Pcoq,Z=f.CLexer,aJ=[0,1],ap=[0,ao[13]],bs=e("Formula.Is_atom"),bu=[0,0,[0,0,0]],bv=e("_"),bx=e("Unify.UFAIL"),bW=e(" : "),bX=e("| "),bY=e(bj),bZ=e(bj),bS=e(" : No such Hint database"),bT=e("Firstorder: "),bL=[0,0],bF=[0,0],bE=[0,0],cm=e("iff"),cn=e("not"),ci=[0,e("Init"),[0,e("Logic"),0]],cj=e("User"),cf=e(bi),b7=e("No link"),b5=e("No axiom link"),b3=[0,e("plugins/firstorder/rules.ml"),61,7],b1=e("Not the expected number of hyps."),cz=e("not implemented ... yet"),cx=e("Untypable instance, maybe higher-order non-prenex quantification"),cw=e(be),cy=e(be),cs=e("can't happen."),ct=e("x"),cB=[0,0],cC=e(bi),dd=e('Deprecated syntax; use "," as separator'),c5=e("Firstorder solver tactic is "),cF=[0,e(aI),[0,e(bc),0]],cG=e("Firstorder Depth"),cJ=[0,e("Congruence"),[0,e(bc),0]],cK=e("Congruence Depth"),cQ=e("Firstorder default solver"),cV=[0,e(_)],cW=e(bl),cX=e(aI),cY=e("Set"),c2=e("Firstorder_Set_Solver"),c6=[0,e("Print"),[0,e(aI),[0,e(bl),0]]],c_=e("Firstorder_Print_Solver"),c$=e("GTauto failed"),de=e("deprecated"),df=e("firstorder-deprecated-syntax"),dh=e(aG),dq=e(aG),dv=e(aD),dy=e(bp),dB=e(bp),dE=e(aD),dK=e(aD),dP=e(aG),dS=e("l'"),dT=e(bn),dV=e(aH),dX=e(_),dY=e(an),d1=e(aH),d2=e(bn),d4=e(_),d5=e(an),d8=e(aH),d_=e(_),d$=e(an),eb=e(an),ee=e(_),ef=e(bd),eh=e(bd),by=f.Assert_failure,bV=f.Constrextern,ck=f.Coqlib,cl=f.UnivGen,b0=f.Control,cv=f.Evarutil,cA=f.Classops,cU=f.Locality,cM=f.Auto,cD=f.Mltop,cP=f.Loc,dg=f.CWarnings;function
bq(h,g,f,e,d,c){var
a=b(h,f,e);return 0===a?b(g,d,c):a}function
br(j,i,h,g,f,e,d,c){var
a=p(j,h,g,f,e);return 0===a?b(i,d,c):a}var
R=[bk,bs,ba(0)];function
aK(g,f){var
b=g,c=f;for(;;){var
d=a(N[26],c);if(6===d[0]){var
e=d[3];if(0<b){var
b=b-1|0,c=e;continue}return 1+aK(0,e)|0}return 0}}function
bt(d,c){var
e=a(aa[28],c[1])[1][6],f=b(ab[4],d,c);function
g(a){return aK(e,a)}return b(j[19][15],g,f)}function
ac(i,c,h,e,f){var
k=b(ab[4],i,e);function
l(i){var
j=a(d[8],i),k=a(aa[28],e[1])[1][8],l=a(O[1][4],k),m=p(x[59],c,l,j,f),n=g(d[92],c,h,m)[2];return b(d[91],c,n)[1]}return b(j[19][15],l,k)}function
aq(c,b,a){return p(S[16],ap[1],c,b,a)}function
ad(m,c,C){var
e=p(S[17],ap[1],m,c,C),n=b(ae[23],c,e);if(n){var
o=n[1],D=o[1];return[0,D,b(d[y][1],-1,o[2])]}var
q=b(ae[21],c,e);if(q){var
r=q[1];return[5,r[2],r[3]]}var
s=b(ae[27],c,e);if(s){var
g=s[1],h=g[2],E=g[3],t=b(d[77],c,g[1]),f=t[1],i=b(d[2][2],c,t[2]),u=a(aa[28],f),k=u[2],v=u[1],w=k[4].length-1;if(0===w)return[1,[0,f,i],h];var
F=0<E?1:0,G=function(e){var
f=v[6],g=a(d[8],e);return b(x[66],c,g)===f?1:0},l=b(j[19][32],G,k[9]);if(!a(ab[19],[0,f,v,k])){var
J=F?l?1:0:1;if(J)return 1===w?[2,[0,f,i],h,l]:[3,[0,f,i],h,l]}return[6,e]}var
z=b(ae[29],c,e);if(z){var
A=z[1],H=A[2],B=b(d[77],c,A[1]),I=B[1];return[4,[0,I,b(d[2][2],c,B[2])],H]}return[6,aq(m,c,e)]}var
bw=[0,a(A[1][6],bv)];function
aL(m,h,q,f,c){var
n=[0,0],k=[0,0],l=[0,0];function
i(B,e,A){var
f=B,o=A;for(;;){var
c=ad(m,h,o);switch(c[0]){case
0:var
C=c[2];i(f,1-e,c[1]);var
o=C;continue;case
1:var
s=1-e,D=s?(n[1]=1,0):s;return D;case
4:var
v=c[2],K=c[1],L=a(q,1),M=a(d[11],L),N=F(ac(m,h,1,K,v),0)[1],P=function(g,j,c){var
h=a(O[1][1][3],c);return i([0,M,f],e,b(d[y][1],g,h))},Q=2-a(j[17][1],v)|0;return p(j[17][87],P,Q,0,N);case
5:var
R=c[2],S=a(q,1),f=[0,a(d[11],S),f],o=R;continue;case
6:var
r=g(d[y][3],f,0,c[1]),w=1-b(d[48],h,r);if(w){if(e){k[1]=[0,r,k[1]];return 0}l[1]=[0,r,l[1]];var
x=0}else
var
x=w;return x;default:var
E=c[2],G=c[1];if(c[3]){var
z=aq(m,h,g(d[y][3],f,0,o));if(e)k[1]=[0,z,k[1]];else
l[1]=[0,z,l[1]]}var
t=ac(m,h,0,G,E),H=function(g,j,c){var
h=a(O[1][1][3],c);return i(f,e,b(d[y][1],g,h))},I=function(b){var
c=1-a(j[17][1],b)|0;return p(j[17][87],H,c,0,b)};if(e)var
J=function(a){return a?0:1},u=b(j[19][32],J,t);else
var
u=e;if(u)n[1]=1;return b(j[19][13],I,t)}}}switch(f){case
0:i(0,0,c);break;case
1:i(0,1,c);break;default:var
e=b(d[90],h,c),o=e[2],r=e[1],s=function(c){var
b=a(q,1);return a(d[11],b)};i(b(j[17][14],s,r),0,o);n[1]=0}return[0,n[1],[0,k[1],l[1]]]}var
n=[0,aJ,ap,bq,br,bt,ac,bw,aL,function(g,f,o,t,e,n){function
k(a){return aq(g,f,a)}try{var
p=a(n,0)+1|0,q=aJ[1]?aL(g,f,n,o,e):bu,r=q[1],u=q[2];if(1===o){var
l=ad(g,f,e);switch(l[0]){case
0:var
h=0;break;case
1:var
h=3;break;case
2:var
h=1;break;case
3:var
h=2;break;case
4:var
x=F(ac(g,f,0,l[1],l[2]),0)[1],y=a(j[17][aF],x),h=[0,p,a(O[1][1][3],y),r];break;case
5:var
h=4;break;default:throw[0,R,l[1]]}var
s=[1,h]}else{var
c=ad(g,f,e);switch(c[0]){case
0:var
m=c[1],z=c[2],A=k(m),b=ad(g,f,m);switch(b[0]){case
0:var
d=[5,b[1],b[2],z];break;case
1:var
d=[0,b[1],b[2]];break;case
2:var
d=[1,b[1],b[2]];break;case
3:var
d=[2,b[1],b[2]];break;case
4:var
d=[4,b[1],b[2]];break;case
5:var
d=[3,m];break;default:var
d=0}var
i=[4,A,d];break;case
1:var
i=0;break;case
2:var
B=c[1];if(c[3])throw[0,R,k(e)];var
i=[0,B];break;case
3:var
C=c[1];if(c[3])throw[0,R,k(e)];var
i=[1,C];break;case
4:var
i=[3,c[1]];break;case
5:var
i=[2,p,c[1],r];break;default:throw[0,R,c[1]]}var
s=[0,i]}var
w=[0,[0,t,k(e),s,u]];return w}catch(a){a=v(a);if(a[1]===R)return[1,a[2]];throw a}}];J(85,n,"Ground_plugin.Formula");var
G=[bk,bx,ba(0)];function
aM(a){return b(d[y][1],-1,a)}function
ar(e,c){function
f(b){var
c=b[1];return[0,c,a(d[al][1],b[2])]}var
g=b(j[17][69],f,e),h=a(d[al][1],c),i=b(x[45],g,h);return a(d[8],i)}function
as(f,W,V){var
i=a(w[2],0),p=[0,0];function
s(c,a){var
d=p[1];function
e(b){var
d=b[1];return[0,d,ar([0,[0,c,a],0],b[2])]}p[1]=[0,[0,c,a],b(j[17][69],e,d)];return 0}function
t(c){var
a=b(d[3],f,c);if(2===a[0]){var
e=a[1];try{var
g=t(b(T[4][2],e,p[1]));return g}catch(a){a=v(a);if(a===C)return c;throw a}}return c}b(w[3],[0,W,V],i);try{for(;;){var
B=a(w[5],i),X=B[2],k=t(b(S[27],f,B[1])),l=t(b(S[27],f,X)),h=b(d[3],f,k),c=b(d[3],f,l);switch(h[0]){case
2:var
q=h[1];if(2===c[0]){var
z=c[1];if(1-(q===z?1:0))if(q<z)s(z,k);else
s(q,l)}else{var
y=ar(p[1],l),$=b(x[37],f,y),at=a(T[2][2],$)?g(x[32],f,q,y)?0:(s(q,y),1):0;if(!at)throw[0,G,k,l]}var
e=3;break;case
6:var
aa=h[3],ab=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
6:var
J=c[3],I=c[2],H=aa,E=ab,e=4;break;default:var
e=2}break;case
7:var
ae=h[3],af=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
7:var
J=c[3],I=c[2],H=ae,E=af,e=4;break;default:var
e=2}break;case
9:var
K=h[2],ag=h[1];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
9:var
L=c[2];b(w[3],[0,ag,c[1]],i);var
M=K.length-1;if(M!==L.length-1)throw[0,G,k,l];var
N=M-1|0,ah=0;if(!(N<0)){var
m=ah;for(;;){var
ai=F(L,m)[m+1],aj=[0,F(K,m)[m+1],ai];b(w[3],aj,i);var
ak=m+1|0;if(N!==m){var
m=ak;continue}break}}var
e=3;break;default:var
e=2}break;case
13:var
O=h[4],al=h[3],am=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
13:var
P=c[4],an=c[3];b(w[3],[0,am,c[2]],i);b(w[3],[0,al,an],i);var
Q=O.length-1;if(Q!==P.length-1)throw[0,G,k,l];var
R=Q-1|0,ao=0;if(!(R<0)){var
n=ao;for(;;){var
ap=F(P,n)[n+1],aq=[0,F(O,n)[n+1],ap];b(w[3],aq,i);var
as=n+1|0;if(R!==n){var
n=as;continue}break}}var
e=3;break;default:var
e=2}break;default:var
e=0}switch(e){case
0:if(2===c[0]){var
D=c[1],u=ar(p[1],k),_=b(x[37],f,u);if(a(T[2][2],_))if(g(x[32],f,D,u))var
A=1;else{s(D,u);var
o=2,A=0}else
var
A=1;if(A)throw[0,G,k,l]}else
if(5===h[0]){var
Z=[0,b(x[60],f,k),l];b(w[3],Z,i);var
o=2}else
var
o=0;break;case
1:var
o=0;break;case
2:var
o=1;break;case
3:var
o=2;break;default:b(w[3],[0,E,I],i);var
ac=aM(J),ad=[0,aM(H),ac];b(w[3],ad,i);var
o=3}switch(o){case
0:if(5===c[0]){var
Y=[0,k,b(x[60],f,l)];b(w[3],Y,i);var
r=1}else
var
r=0;break;case
1:var
r=0;break;case
2:var
r=1;break;default:var
r=2}switch(r){case
0:if(1-g(d[96],f,k,l))throw[0,G,k,l];var
U=0;break;case
1:var
U=0;break;default:var
U=1}continue}}catch(a){a=v(a);if(a===w[1])return p[1];throw a}}function
bz(a,g,c){function
e(c){if(b(d[48],a,c))if(b(d[67],a,c)===g)return 0;function
h(a,c){var
b=e(c);return 0<=a?0<=b?a+b|0:a:b}var
f=p(d[106],a,h,-1,c);return 0<=f?f+1|0:-1}return e(c)}function
bA(g,f){var
c=[0,1],e=[0,0];function
h(f,i){var
j=b(d[3],g,i);if(2===j[0]){var
k=j[1];try{var
n=f+b(T[4][2],k,e[1])|0,o=a(d[9],n);return o}catch(b){b=v(b);if(b===C){var
l=c[1];c[1]++;e[1]=[0,[0,k,l],e[1]];return a(d[9],l+f|0)}throw b}}function
m(a){return a+1|0}return z(d[102],g,m,h,f,i)}var
i=h(0,f);return[0,c[1]-1|0,i]}function
bB(a,f,e,c,i){try{var
j=as(a,c,i),g=b(T[4][2],f,j);if(b(d[48],a,g))var
h=[0,[1,e]];else
var
k=bz(a,f,c),h=[0,[0,bA(a,g),k]];return h}catch(a){a=v(a);if(a[1]===G)return 0;if(a===C)return[0,[1,e]];throw a}}function
aN(f,e,c){function
g(b){return a(d[11],f+b|0)}var
h=b(j[17][56],e,g);return b(d[y][4],h,c)}var
af=[0,G,as,bB,function(f,e,c){var
a=e[1],g=c[2],h=c[1],i=aN(0,a,e[2]),k=aN(a,h,g);try{var
l=as(f,i,k),m=function(c){var
e=c[1]<a?1:0,g=c[2];return e?e:b(d[48],f,g)},n=b(j[17][21],m,l);return n}catch(a){a=v(a);if(a[1]===G)return 0;throw a}}];J(90,af,"Ground_plugin.Unify");function
aO(a){if(0===a[0]){var
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
0:return am;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
0:return am;case
1:return 40;case
2:return-15;case
3:return-50;default:return am}return-29}var
bC=[0,function(b,a){var
c=aO(a[3]);return aO(b[3])-c|0}],bD=[0,function(c,a){var
e=a[2],f=c[2],d=b(aP[17][1],c[1],a[1]);if(0===d){var
h=function(c,a){var
d=t.caml_int_compare(c[1],a[1]),e=a[2],f=c[2];return 0===d?b(N[80],f,e):d};return g(U[5],h,f,e)}return d}],k=a(j[21][1],[0,N[80]]),V=a(j[20][1],bD);function
ag(h,a,f,c){var
e=g(d[5],bE,h,a);try{var
i=[0,f,b(k[22],e,c)],j=g(k[4],e,i,c);return j}catch(a){a=v(a);if(a===C)return g(k[4],e,[0,f,0],c);throw a}}function
aQ(i,h,f,a){var
c=g(d[5],bF,i,h);try{var
l=b(k[22],c,a),m=function(a){return 1-b(A[68][1],a,f)},e=b(j[17][61],m,l),n=e?g(k[4],c,e,a):b(k[6],c,a);return n}catch(b){b=v(b);if(b===C)return a;throw b}}var
K=a(at[2],bC);function
bG(a){return[0,a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]-1|0]}function
bH(c,a){var
d=a[8],e=b(V[4],c,a[7]);return[0,a[1],a[2],a[3],a[4],a[5],a[6],e,d]}function
bI(m,c,e){var
f=b(V[3],c,e[7]);if(f)var
h=f;else{var
i=c[2],n=c[1];if(i){var
j=i[1],k=j[1],o=j[2],l=function(c){var
e=c[2],p=c[1];if(e){var
f=e[1],h=f[1],q=f[2],i=b(A[68][1],n,p);if(i){var
j=k<h?1:0;if(j){var
r=[0,k,a(d[8],o)],s=[0,h,a(d[8],q)];return g(af[4],m,s,r)}var
l=j}else
var
l=i;return l}return 0};return b(V[17],l,e[7])}var
h=0}return h}function
au(j,g,f,e,i,a){var
h=L(n[9],j,g,f,e,i,a[6]);if(0===h[0]){var
c=h[1];if(1===f){var
k=a[8],l=a[7],m=a[6],o=c[2],p=a[3],q=a[2];return[0,b(K[2],c,a[1]),q,p,o,0,m,l,k]}var
r=a[8],s=a[7],t=a[6],u=a[5],v=a[4],w=a[3],x=ag(g,c[2],e,a[2]);return[0,b(K[2],c,a[1]),x,w,v,u,t,s,r]}var
d=h[1];if(1===f)return[0,a[1],a[2],a[3],d,[0,d],a[6],a[7],a[8]];var
y=a[8],z=a[7],A=a[6],B=a[5],C=a[4],D=[0,d,a[3]],E=ag(g,d,e,a[2]);return[0,a[1],E,D,C,B,A,z,y]}function
bJ(c,b,a){function
d(a,b){return a[1]===n[7]?b:ag(c,a[2],a[1],b)}var
e=a[8],f=a[7],h=a[6],i=a[5],k=a[4],l=a[3],m=g(j[17][16],d,b,a[2]);return[0,g(j[17][16],K[2],b,a[1]),m,l,k,i,h,f,e]}function
bK(f,e,c){var
h=c[2],i=g(d[5],bL,f,e),l=b(k[22],i,h);return a(j[17][5],l)}function
bM(g,f){var
b=f;for(;;){var
c=a(K[3],b[1]),d=a(K[4],b[1]);if(c[1]===n[7]){var
e=[0,d,b[2],b[3],b[4],b[5],b[6],b[7],b[8]];if(b[4]===c[2])return[0,c,e];var
b=e;continue}var
h=b[8],i=b[7],j=b[6],k=b[5],l=b[4],m=b[3];return[0,c,[0,d,aQ(g,c[2],c[1],b[2]),m,l,k,j,i,h]]}}function
bN(e){var
b=[0,-1],f=V[1];function
c(a){if(a)b[1]++;return b[1]}var
g=a(d[11],1);return[0,K[1],k[1],0,g,0,c,f,e]}function
bO(c){if(2===c[0]){var
d=c[1],e=function(a){return[3,[0,d,a+1|0]]},f=a(ab[21],d);return b(j[17][56],f,e)}return[0,c,0]}var
bP=a(j[17][78],bO);function
bQ(b,e,d,c){var
f=a(bP,d);function
h(c,a){var
g=a[1],d=L(av[162],0,0,0,b,a[2],c),e=p(ah[2],0,b,d[1],d[2]),f=e[1];return[0,au(b,f,0,c,e[2],g),f]}return g(j[17][16],h,f,[0,c,e])}function
bR(e,c,h,f){var
d=[0,f];function
i(i){var
f=a(aw[30],i[7]);switch(f[0]){case
0:case
2:case
3:var
h=f[1][1][1];try{var
j=b(x[bf],c,h)[1],k=g(ah[1],e,c,h);d[1]=au(e,c,2,j,k,d[1]);var
l=0;return l}catch(a){a=v(a);if(a===C)return 0;throw a}default:return 0}}function
k(d,c,a){return b(j[17][11],i,a)}function
m(d){try{var
c=a(aw[15],d),e=c}catch(c){c=v(c);if(c!==C)throw c;var
f=b(P[17],d,bS),h=b(P[17],bT,f),i=a(l[3],h),e=g(W[6],0,0,i)}return b(aw[14][12],k,e)}b(j[17][11],m,h);return[0,d[1],c]}function
bU(c){function
e(g,f,e){var
c=a(aa[2],0),h=a(av[17],c),i=a(d[8],g),j=z(bV[6],0,0,c,h,i),k=a(l[14],0),m=a(ax[20],j),n=a(l[3],bW),o=b(l[37],ai[58],f),p=a(l[3],bX),q=b(l[12],p,o),r=b(l[12],q,n),s=b(l[12],r,m),t=b(l[12],s,k);return b(l[12],t,e)}var
f=a(l[3],bY),h=a(l[7],0),i=g(k[11],e,c,h),j=a(l[14],0),m=a(l[3],bZ),n=b(l[12],m,j),o=b(l[12],n,i),p=b(l[12],o,f);return b(l[24],0,p)}var
q=[0,[0,k[1],k[2],k[3],k[4],k[5],k[6],k[7],k[8],k[9],k[10],k[11],k[12],k[13],k[14],k[15],k[16],k[17],k[18],k[19],k[20],k[21],k[22],k[23],k[24]],V,ag,aQ,K,bG,bH,bI,au,bJ,bK,bM,bN,bQ,bR,bU];J(bf,q,"Ground_plugin.Sequent");function
u(t,m,k,s){function
c(c){a(b0[3],0);var
u=a(i[67][3],c),d=a(o[42][5],c),e=a(o[42][4],c);function
r(v,u,t){var
i=v,h=u,f=t;for(;;){if(0<i){if(h){var
n=h[2],k=h[1],m=a(O[2][1][1],k),w=a(o[42][6],c);if(!p(x[34],d,e,m,w)){var
y=g(x[35],d,e,m);if(!b(j[17][22],y,f)){var
z=r(i-1|0,n,[0,k,f]),A=a(O[2][1][3],k);return L(q[9],d,e,0,[0,m],A,z)}}var
i=i-1|0,h=n,f=[0,k,f];continue}var
B=a(l[3],b1);return g(W[3],0,0,B)}return s}}var
f=r(t,u,0);if(m)var
v=a(o[42][6],c),h=L(q[9],d,e,1,n[7],v,f);else
var
h=f;return a(k,h)}return a(i[67][8],c)}function
b2(a){if(0===a[0])return a[1];throw[0,by,b3]}function
D(b){return 0===b[0]?a(h[75],[0,b[1],0]):c[66][2]}function
b4(e,d){function
f(f){try{var
k=function(b){return a(h[42],b)},m=a(o[42][4],f),n=g(q[11],m,e,d),p=a(c[66][61],n),r=b(i[72][1],p,k);return r}catch(d){d=v(d);if(d===C){var
j=a(l[3],b5);return b(c[66][4],0,j)}throw d}}return a(i[67][9],f)}function
b6(m,k,f,j,e){var
n=u(1,0,j,e),o=[0,h[16],0],p=[0,D(f),o];function
r(k){try{var
p=g(q[11],k,m,e),r=a(i[16],p),j=r}catch(d){d=v(d);if(d!==C)throw d;var
n=a(l[3],b7),j=b(c[66][4],0,n)}function
o(e){function
g(e){function
g(b){var
c=[0,a(d[21],[0,b,[0,e]]),0];return a(h[M],c)}var
j=a(c[66][61],f);return b(i[72][1],j,g)}var
j=a(c[66][61],e);return b(i[72][1],j,g)}return b(i[72][1],j,o)}var
s=[0,b(i[72][1],i[55],r),p],t=a(c[66][22],s);return g(c[66][27],t,n,k)}function
b8(d,b,a){var
e=u(0,1,b,a);return g(c[66][27],h[bo],e,d)}function
b9(f,e,d){var
g=u(0,1,e,d),i=[0,a(c[66][34],g)],j=b(h[aF],0,i);return b(c[66][12],j,f)}function
b_(f,e,d){var
i=u(1,1,e,d),j=a(c[66][34],i),k=b(c[66][3],h[17],j),l=b(c[66][12],k,f),m=u(1,1,e,d);return g(c[66][27],h[16],m,l)}function
b$(l,k,d,j,f){function
e(m){var
p=a(o[42][5],m),e=F(b(n[5],p,l),0)[1],q=u(e,0,j,f),r=[0,b(c[66][31],e,h[16]),0],s=[0,D(d),r],t=h[99],v=a(c[66][61],d),w=[0,b(i[72][1],v,t),s],x=a(c[66][22],w);return g(c[66][27],x,q,k)}return a(i[67][9],e)}function
ca(l,e,d,k,f){function
m(m){var
p=a(o[42][5],m),q=b(n[5],p,l);function
r(e){var
g=[0,u(e,0,k,f),0],i=[0,b(c[66][31],e,h[16]),g],j=[0,D(d),i];return a(c[66][22],j)}var
s=b(j[19][15],r,q),t=h[99],v=a(c[66][61],d),w=b(i[72][1],v,t);return g(c[66][28],w,s,e)}return a(i[67][9],m)}function
cb(d){var
e=h[99],f=a(c[66][61],d);return b(i[72][1],f,e)}function
cc(e,l,s,k,r,q){var
t=e[2],v=e[1];function
f(m){var
w=a(o[42][4],m),x=a(o[42][5],m),p=z(n[6],x,w,0,e,l),f=p.length-1,A=a(j[19][12],l),B=u(f,0,r,q),C=[0,b(c[66][31],f,h[16]),0],E=[0,D(k),C];function
G(r){function
c(e){var
f=F(p,e)[e+1],c=a(j[17][1],f),g=[0,[0,v,e+1|0],a(d[2][1],t)],h=[0,a(d[28],g),A],i=a(d[21],h);function
k(b){return a(d[9],c-b|0)}var
l=b(j[19][2],c,k),m=[0,b(d[y][1],c,i),l],n=[0,a(d[21],m)],o=[0,b(d[y][1],c,r),n],q=a(d[21],o);return b(d[38],q,f)}var
e=b(j[17][56],f,c);return a(h[M],e)}var
H=a(c[66][61],k),I=[0,b(i[72][1],H,G),E],J=a(c[66][22],I);return g(c[66][27],J,B,s)}return a(i[67][9],f)}function
cd(k,j,m,l,e,g,f){var
n=[0,0,k,b(d[y][1],1,j)],o=a(d[18],n),r=u(2,1,g,f),s=[0,a(c[66][34],r),0],t=[0,h[17],[0,h[17],s]],p=0,q=0,v=[0,D(e),t];function
w(i){var
c=a(d[9],2),e=[0,0,b(d[y][1],1,k),c],f=[0,i,[0,a(d[19],e)]],g=[0,0,j,a(d[21],f)],l=[0,a(d[19],g),0];return a(h[M],l)}var
x=a(c[66][61],e),z=[0,b(i[72][1],x,w),v],A=[0,a(c[66][22],z),q];function
B(b){return a(h[42],b)}var
C=a(c[66][61],e),E=[0,b(i[72][1],C,B),A],F=a(h[$],o),G=[0,b(c[66][21],F,E),p],H=[0,u(1,0,g,f),0],I=[0,D(e),H],J=[0,a(c[66][22],[0,h[17],I]),G],K=a(h[$],m),L=b(c[66][21],K,J);return b(c[66][12],L,l)}function
ce(f,e,d){if(n[1][1])var
j=a(l[3],cf),i=b(c[66][4],0,j);else
var
i=f;var
k=u(0,1,e,d),m=a(c[66][34],k),o=b(c[66][3],h[17],m),p=b(c[66][12],o,f),q=u(0,1,e,d),r=g(c[66][27],h[16],q,p);return b(c[66][12],r,i)}function
cg(l,k,d,j,f){function
e(m){var
p=a(o[42][5],m),e=F(b(n[5],p,l),0)[1],q=[0,u(e-1|0,0,j,f),0],r=[0,b(c[66][31],e,h[16]),q],s=[0,D(d),r],t=a(c[66][22],s),v=h[99],w=a(c[66][61],d),x=b(i[72][1],w,v);return g(c[66][27],x,t,k)}return a(i[67][9],e)}function
ch(l,k,g,f,e){var
m=u(0,1,f,a(q[6],e)),n=[0,a(c[66][34],m),0],p=u(1,0,f,a(q[6],e)),r=[0,a(c[66][34],p),0],s=[0,h[16],r],t=[0,D(g),s];function
v(f){function
e(g){var
i=a(o[42][12],g),e=b(j[17][7],i,0),k=[0,f,[0,a(d[10],e)]],l=a(d[21],k),m=a(h[75],[0,e,0]),n=a(h[M],[0,l,0]);return b(c[66][3],n,m)}return a(i[67][9],e)}var
w=a(c[66][61],g),x=[0,b(i[72][1],w,v),t],y=[0,a(c[66][22],[0,h[16],x]),n],z=a(h[$],l),A=b(c[66][21],z,y);return b(c[66][12],A,k)}function
aR(b){var
c=g(ck[2],cj,ci,b);return a(cl[21],c)}var
H=[aE,function(e){var
b=aR(cm),c=[0,[0,0,[1,a(N[66],b)[1]]],0],d=aR(cn);return[0,[0,0,[1,a(N[66],d)[1]]],c]}];function
co(e){var
f=a(o[42][12],e);function
g(d){var
c=bb(H),e=[0,d,1],f=bh===c?H[1]:aE===c?a(aS[2],H):H;return b(h[69],f,e)}var
d=bb(H),j=b(c[66][23],g,f),k=bh===d?H[1]:aE===d?a(aS[2],H):H,l=a(h[68],k);return b(i[72][2],l,j)}var
s=[0,u,b2,D,b4,b6,b8,b9,b_,b$,ca,cb,cc,cd,ce,cg,ch,a(i[67][9],co)];J(112,s,"Ground_plugin.Rules");function
cp(e,c){function
f(e,c){var
f=a(d[al][1],c),g=a(d[al][1],e);return b(N[80],g,f)}if(0===e[0]){var
g=e[1],h=g[1],j=e[2],k=g[2];if(0===c[0]){var
i=c[1],l=c[2],m=i[2],o=i[1],p=function(b,a){return b-a|0},q=function(b,a){return b-a|0},r=b(n[3],q,p);return L(b(n[4],r,f),o,h,j,l,k,m)}return 0===h?1:-1}var
s=e[1];return 0===c[0]?0===c[1][1]?-1:1:f(s,c[1])}function
cq(c,a){return c===a?0:c===n[7]?1:a===n[7]?-1:b(aP[17][1],c,a)}var
cr=[0,function(c,a){var
d=a[2],e=a[1],f=c[2],g=c[1];return p(b(n[3],cp,cq),e,g,d,f)}],aj=a(j[20][1],cr);function
aT(F,c,m){var
d=[0,aj[1]];function
e(f){var
i=f[3];if(0===i[0]){var
c=i[1];if(typeof
c==="number")var
n=1;else
if(2===c[0])var
w=c[3],k=c[2],v=c[1],h=1,n=0;else
var
n=1;if(n)var
h=0}else{var
e=i[1];if(typeof
e==="number")var
h=0;else
var
w=e[3],k=e[2],v=e[1],h=1}if(h){var
x=f[4],p=[0,1],r=[0,w],D=f[1],s=function(c,a){function
e(f,e){var
a=z(af[3],F,v,k,f,e);if(a){var
c=a[1];return 0===c[0]?(p[1]=0,d[1]=b(aj[4],[0,c,D],d[1]),0):(r[1]=1,0)}return 0}var
f=c[1];function
g(c){var
d=a[2];function
f(a){return e(c,a)}return b(j[17][11],f,d)}b(j[17][11],g,f);var
h=c[2];function
i(c){var
d=a[1];function
f(a){return e(c,a)}return b(j[17][11],f,d)}return b(j[17][11],i,h)},A=m[1],B=function(a){return s(x,a[4])};b(q[5][5],B,A);var
o=m[5],y=o?[0,o[1],0]:0;s(x,[0,y,m[3]]);var
t=p[1],u=t?r[1]:t,E=u?(d[1]=b(aj[4],[0,[1,k],f[1]],d[1]),0):u;return E}var
C=a(l[3],cs);return g(W[3],0,0,C)}b(j[17][11],e,c);return a(aj[21],d[1])}function
aU(f,c){try{var
g=b(q[12],f,c),h=g[1],a=h[3],l=g[2];if(0===a[0]){var
i=a[1];if(typeof
i==="number")var
e=1;else
if(2===i[0])var
d=1,e=0;else
var
e=1;if(e)var
d=0}else
var
d=typeof
a[1]==="number"?0:1;if(d)var
k=aU(f,l),j=[0,[0,h,k[1]],k[2]];else
var
j=[0,0,c];return j}catch(a){a=v(a);if(a===at[1])return[0,0,c];throw a}}var
aV=a(A[1][6],ct);function
cu(i,c,w,v,f,u){if(w===n[7])var
p=aV;else
var
E=g(ah[1],i,c,v),F=g(S[28],i,c,E),t=b(d[71],c,F)[1],G=t?t[1]:aV,p=G;function
x(b){return a(d[9],f-b|0)}var
z=b(j[17][56],f,x),B=b(d[y][4],z,u),o=f,m=A[1][10][1],e=i,l=c,k=0;for(;;){if(0===o)return[0,l,k,B];var
q=g(h[13],m,p,e),r=ei(cv[7],0,0,0,0,0,e,l,av[116]),s=[0,[0,q],r[2][1]],C=r[1],D=b(d[aF],s,e),o=o-1|0,m=b(A[1][10][4],q,m),e=D,l=C,k=[0,s,k];continue}}var
Q=[0,aU,aT,function(f,e,r,k){function
m(m){var
t=aT(a(o[42][4],m),f,k);function
u(f){if(f[2]===n[7])var
e=f[1],m=function(g,k){function
f(F){if(0===e[0]){var
f=e[1];if(0===f[1]){var
m=f[2],n=a(q[6],k),r=[0,p(s[1],0,1,g,n),0],t=a(c[66][35],r),u=a(h[bg],[0,[0,m,0]]);return b(c[66][3],u,t)}var
v=a(l[3],cz);return b(c[66][4],0,v)}var
w=e[1],x=[0,a(c[66][24],h[41]),0],y=a(q[6],k),z=[0,p(s[1],0,1,g,y),0],A=[0,a(c[66][35],z),0];function
B(c){var
e=a(o[42][12],c),f=b(j[17][7],e,0),g=[0,[0,a(d[10],f),0]];return a(h[bg],g)}var
C=[0,a(i[67][9],B),A],D=[0,a(c[66][22],[0,h[17],C]),x],E=a(h[$],w);return b(c[66][21],E,D)}return a(i[67][9],f)};else
var
m=function(x,k){var
e=f[2],m=f[1];function
n(y){var
f=a(o[42][4],y);if(0===m[0]){var
t=m[1],n=t[2],r=t[1],u=[0,r,g(d[5],0,f,n)];if(g(q[8],f,[0,e,[0,u]],k)){var
z=a(l[3],cw);return b(c[66][4],0,z)}if(0<r)var
A=function(j){function
c(c){var
q=a(o[42][4],c),f=cu(a(o[42][5],c),q,e,j,r,n),s=f[2],t=f[1],u=a(d[21],[0,j,[0,f[3]]]),k=b(d[38],u,s);try{var
A=a(o[42][5],c),B=p(ah[2],0,A,t,k),m=B}catch(b){b=v(b);if(!a(W[18],b))throw b;var
w=a(l[3],cx),m=g(W[6],0,0,w)}var
x=m[1],y=a(h[M],[0,k,0]),z=a(i[65][1],x);return b(i[18],z,y)}return a(i[67][9],c)},B=a(c[66][61],e),w=b(i[72][1],B,A);else
var
G=function(b){var
c=[0,a(d[21],[0,b,[0,n]]),0];return a(h[M],c)},H=a(c[66][61],e),w=b(i[72][1],H,G);var
C=b(q[7],[0,e,[0,u]],k),D=a(q[6],C),E=[0,p(s[1],1,0,x,D),0],F=[0,a(c[66][35],E),0];return a(c[66][22],[0,w,[0,h[17],F]])}var
I=m[1];if(g(q[8],f,[0,e,0],k)){var
J=a(l[3],cy);return b(c[66][4],0,J)}var
K=[0,a(c[66][24],h[41]),0],L=b(q[7],[0,e,0],k),N=a(q[6],L),O=[0,p(s[1],1,0,x,N),0],P=[0,a(c[66][35],O),0],Q=[0,h[17],P];function
R(c){function
e(e){var
f=a(o[42][12],e),g=b(j[17][7],f,0),i=[0,c,[0,a(d[10],g)]],k=[0,a(d[21],i),0];return a(h[M],k)}return a(i[67][9],e)}var
S=a(c[66][61],e),T=[0,b(i[72][1],S,R),Q],U=[0,a(c[66][22],[0,h[17],T]),K],V=a(h[$],I);return b(c[66][21],V,U)}return a(i[67][9],n)};return m(r,k)}var
w=b(j[17][69],u,t),x=a(c[66][26],w);return b(c[66][12],x,e)}return a(i[67][9],m)}];J(114,Q,"Ground_plugin.Instances");var
ay=[0,function(_,k){function
d(m){function
d(a,d){var
c=d[1];return 1===c[0]?b(A[18][7],c[1],a):a}var
e=a(cA[24],0),f=g(aW[20],d,A[18][1],e),h=a(A[18][12],f),j=b(ao[7][13],ao[13],[0,A[1][12][2],h]);n[2][1]=j;function
B(C,k){function
d(w){if(t.caml_equal(a(r[13][8],0),cB)){var
$=a(o[42][4],w),aa=[0,a(i[67][13],w),$],ab=g(ai[84],0,0,aa);b(aX[10],0,ab)}try{var
ad=a(o[42][4],w),I=b(q[12],ad,k),h=I[2],j=I[1],e=function(b){var
c=a(o[42][4],w);return g(q[10],c,C,b)},ae=0,f=function(a){return B(ae,a)},d=B([0,j,C],h),D=j[3];if(0===D[0]){var
u=D[1];if(typeof
u==="number")var
x=a(s[11],j[1]);else
switch(u[0]){case
0:var
af=u[1],ag=e(h),x=z(s[9],af,d,j[1],f,ag);break;case
1:var
ah=u[1],aj=e(h),x=z(s[10],ah,d,j[1],f,aj);break;case
2:var
ak=a(o[42][4],w),K=b(Q[1],ak,k),M=K[1],al=K[2],N=B(b(P[26],M,C),al);if(n[1][1])if(0<k[8])var
am=e(k),O=p(Q[3],M,N,f,am),E=1;else
var
E=0;else
var
E=0;if(!E)var
O=N;var
x=O;break;case
3:var
an=u[1];if(n[1][1])var
ao=e(h),R=z(s[15],an,d,j[1],f,ao);else
var
R=d;var
x=R;break;default:var
m=u[2],ap=u[1];if(typeof
m==="number")var
A=d;else
switch(m[0]){case
3:var
av=m[1];if(0<k[8])if(n[1][1])var
aw=e(h),S=z(s[16],av,d,j[1],f,aw),F=1;else
var
F=0;else
var
F=0;if(!F)var
S=d;var
A=S;break;case
4:var
ax=m[2],ay=m[1];if(n[1][1])var
az=e(h),T=L(s[12],ay,ax,d,j[1],f,az);else
var
T=d;var
A=T;break;case
5:var
aA=m[3],aB=m[2],aC=m[1],aD=e(h),A=ej(s[13],aC,aB,aA,d,j[1],f,aD);break;default:var
ar=m[2],as=m[1],au=e(h),A=L(s[12],as,ar,d,j[1],f,au)}var
aq=e(h),x=z(s[5],ap,A,j[1],f,aq)}var
J=x}else{var
U=D[1];if(typeof
U==="number")switch(U){case
0:var
aE=e(h),y=g(s[8],d,f,aE);break;case
1:var
aF=e(h),y=g(s[6],d,f,aF);break;case
2:var
aG=e(h),y=g(s[7],d,f,aG);break;case
3:var
y=d;break;default:if(n[1][1])var
aH=a(l[3],cC),V=b(c[66][4],0,aH);else
var
V=d;var
aI=e(h),y=g(s[14],V,f,aI)}else{var
aJ=a(o[42][4],w),W=b(Q[1],aJ,k),X=W[1],aK=W[2],Y=B(b(P[26],X,C),aK);if(n[1][1])if(0<k[8])var
aL=e(k),Z=p(Q[3],X,Y,f,aL),G=1;else
var
G=0;else
var
G=0;if(!G)var
Z=Y;var
y=Z}var
J=y}var
H=J}catch(a){a=v(a);if(a!==at[1])throw a;var
H=_}var
ac=b(s[4],k[4],k);return b(c[66][12],ac,H)}return a(i[67][9],d)}var
u=a(i[67][3],m),w=a(aW[1],u);return a(k,function(a){var
b=0;function
c(a){return B(b,a)}return p(s[1],w,1,c,a)})}return a(i[67][9],d)}];J(bo,ay,"Ground_plugin.Ground");a(cD[10],ak);var
X=[0,3];function
cE(a){return a?(X[1]=b(P[6],a[1],0),0):(X[1]=3,0)}var
cH=[0,0,cG,cF,function(a){return[0,X[1]]},cE];b(aY[3],0,cH);var
az=[0,am];function
cI(a){return a?(az[1]=b(P[6],a[1],0),0):(az[1]=0,0)}var
cL=[0,1,cK,cJ,function(a){return[0,az[1]]},cI];b(aY[3],0,cL);var
cN=[0,aZ,0],cO=[0,function(b,a){return p(cM[14],0,0,0,0)}];g(r[4][16],0,aZ,cO);var
a0=[31,b(cP[11],0,[0,cN,0])],aA=b(r[15][1],[0,a0],cQ),a1=aA[3],a2=aA[2],a3=aA[1],cR=0,cS=0;function
cT(e,d,c){var
f=a(r[9][3],e);b(a3,a(cU[5],d[2]),f);return c}var
cZ=[0,[0,0,[0,cY,[0,cX,[0,cW,[1,cV,[5,a(m[16],r[2][8])],0]]]],cT,cS],cR],c0=0,c1=[0,function(a){return a4[4]}];p(a5[10],c2,c1,c0,cZ);var
c3=0,c4=0,c7=[0,[0,0,c6,function(g,c){var
d=a(a1,0),e=a(l[3],c5),f=b(l[12],e,d);b(aX[6],0,f);return c},c4],c3],c8=0,c9=[0,function(a){return a4[3]}];p(a5[10],c_,c9,c8,c7);var
da=a(l[3],c$),db=b(c[66][4],0,da);function
Y(h,d,g,f){var
e=n[1][1];function
j(a){var
c=a[2],d=a[1];n[1][1]=e;return b(i[21],[0,c],d)}function
k(m){n[1][1]=h;var
j=d?d[1]:a(a2,0)[2];function
k(h){function
d(d){var
j=a(q[13],X[1]),k=a(o[42][4],d),l=a(o[42][5],d),m=p(q[14],l,k,g,j)[1],n=a(o[42][4],d),r=a(o[42][5],d),e=p(q[15],r,n,f,m),s=e[2],t=a(h,e[1]),u=a(i[65][1],s);return b(c[66][3],u,t)}return a(i[67][9],d)}var
l=b(ay[1],j,k);n[1][1]=e;return l}var
l=a(i[67][9],k);return b(i[22],l,j)}function
a6(d,c,b){return a(r[5][28],ax[11])}function
a7(f,e,d){function
b(b){return a(ai[58],b[2])}var
c=a(ax[3],b);return a(r[5][28],c)}function
a8(d,c,b){return a(r[5][28],ai[58])}function
dc(b){return a(l[22],dd)}var
a9=p(dg[1],df,de,0,dc),B=a(m[2],dh);function
di(c,d){var
e=a(m[18],I[18]),f=a(m[4],e),g=b(m[7],f,d),h=b(r[9][10],c,g),i=a(m[18],I[18]),j=a(m[5],i);return[0,c,b(m[8],j,h)]}b(a_[9],B,di);function
dj(d,c){var
e=a(m[18],I[18]),f=a(m[5],e),g=b(m[7],f,c),h=b(r[3][2],d,g),i=a(m[18],I[18]),j=a(m[5],i);return b(m[8],j,h)}b(a_[10],B,dj);function
dk(d,c){var
e=a(m[18],I[18]),f=a(m[5],e),g=b(m[7],f,c);return b(r[13][10],d,g)}b(aB[7],B,dk);var
dl=a(m[18],I[18]),dm=a(m[6],dl),dn=[0,a(aB[3],dm)];b(aB[4],B,dn);var
dp=a(m[4],B),aC=g(E[16],E[13],dq,dp),dr=0,ds=0;function
dt(a,c,b){return[0,a,0]}var
du=[6,E[17][15]],dw=[0,[0,[0,[0,0,[0,a(Z[10],dv)]],du],dt],ds];function
dx(b,e,a,d,c){return[0,a,b]}var
dz=[0,a(Z[10],dy)],dA=[2,[6,E[17][15]],dz],dC=[0,a(Z[10],dB)],dD=[6,E[17][15]],dF=[0,[0,[0,[0,[0,[0,0,[0,a(Z[10],dE)]],dD],dC],dA],dx],dw];function
dG(d,c,a,f,e){b(a9,0,0);return[0,a,[0,c,d]]}var
dH=[3,[6,E[17][15]]],dI=[6,E[17][15]],dJ=[6,E[17][15]],dL=[0,[0,[0,[0,[0,[0,0,[0,a(Z[10],dK)]],dJ],dI],dH],dG],dF],dM=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],dL]],dr]];g(E[21],aC,0,dM);p(r[5][1],B,a6,a7,a8);var
dN=[0,aC,0];function
dO(c){var
d=c[2],e=a(m[4],B);return[0,b(m[7],e,d)]}g(r[10][5],dP,dO,dN);var
dQ=0;function
dR(f,e,d,c){var
g=a(r[13][24],c);return Y(1,b(U[16],g,f),e,d)}var
dU=[0,dT,[1,[0,[5,a(m[16],I[17])]],dS,0]],dW=[1,[5,a(m[16],B)],dV,dU],dZ=[0,[0,[0,dY,[1,[4,[5,a(m[16],r[2][8])]],dX,dW]],dR],dQ];function
d0(e,d,c){var
f=a(r[13][24],c);return Y(1,b(U[16],f,e),0,d)}var
d3=[0,d2,[1,[0,[5,a(m[16],I[17])]],d1,0]],d6=[0,[0,[0,d5,[1,[4,[5,a(m[16],r[2][8])]],d4,d3]],d0],dZ];function
d7(e,d,c){var
f=a(r[13][24],c);return Y(1,b(U[16],f,e),d,0)}var
d9=[1,[5,a(m[16],B)],d8,0],ea=[0,[0,[0,d$,[1,[4,[5,a(m[16],r[2][8])]],d_,d9]],d7],d6];z(r[10][8],ak,eb,0,0,ea);var
ec=0;function
ed(d,c){var
e=a(r[13][24],c);return Y(0,b(U[16],e,d),0,0)}var
eg=[0,[0,[0,ef,[1,[4,[5,a(m[16],r[2][8])]],ee,0]],ed],ec];z(r[10][8],ak,eh,0,0,eg);var
a$=[0,ak,X,a0,a3,a2,a1,db,Y,a6,a7,a8,a9,B,aC];J(134,a$,"Ground_plugin.G_ground");J(135,[0,n,af,q,s,Q,ay,a$],"Ground_plugin");return}
