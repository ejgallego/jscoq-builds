(function(fy){"use strict";var
bi=125,aT=".",bj=112,cd="cc",H=108,cj="$l",b7="  [",b5=3901498,b6=" : ",ci=-431191102,cc="$n",bh=-912009552,P=250,cb=105,X="Init",be=127,G=246,aW="congruence",ca="[",aY=115,aS=130,aV="plugins/cc/g_congruence.ml4",aR=111,b4="cc_plugin",aU="Extension: cannot occur",b3="A",D=120,bg=122,b$="X",cg=117,ch="with",aX="]",aj=118,b2=915186972,b_=888453194,cf=135,W="Logic",b8="Congruence",b9=" and ",C=109,Q=136,b1=-318868643,ao=126,b0=147,ce="Heq",aQ="f_equal",aP=15500,bf=1e3,w=fy.jsoo_runtime,q=w.caml_check_bound,ay=w.caml_int_compare,aO=w.caml_make_vect,d=w.caml_new_string,O=w.caml_obj_tag,az=w.caml_register_global,r=w.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
B(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
fx(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
bd(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
i=w.caml_get_global_data(),ai=d(b4),e=i.Term,k=i.Names,ak=i.Hashset,bl=i.Sorts,h=i.Util,s=i.Not_found,aC=i.Global,ar=i.Environ,l=i.Tacmach,g=i.Int,t=i.Queue,c=i.Pp,a7=i.Control,A=i.CErrors,by=i.Vars,bx=i.Goal,as=i.Assert_failure,K=i.Termops,a0=i.Feedback,ap=i.Hashtbl,R=i.Pervasives,L=i.CamlinternalLazy,av=i.Globnames,m=i.Proofview,p=i.Tacticals,F=i.Tactics,au=i.Typing,bS=i.Evarsolve,aN=i.Refiner,ba=i.Coqlib,bV=i.Universes,aL=i.Evd,bP=i.Context,$=i.CClosure,ax=i.Loc,ac=i.Ltac_plugin,ab=i.Stdarg,an=i.Genarg,bc=i.Mltop,aZ=[0,0],dP=d("Out of depth ... "),dO=d("Out of instances ... "),dQ=d("First run was incomplete, completing ... "),dN=d("Executing ... "),dM=d("Running E-matching algorithm ... "),dL=d("paf_of_patt: pattern is trivial"),dJ=d("wrong incomplete class"),dD=d(" ... "),dE=d(" = "),dF=d("Checking if "),dC=d("Yes"),dG=d("No"),dz=d(aT),dA=d("Processing mark for term "),dw=d("weird error in injection subterms merge"),dx=[0,d("add_pacs")],du=d(aT),dv=d("Updating term "),dr=d(aT),ds=d(b9),dt=d("Merging "),dm=d(aT),dn=d(b9),dp=d("Linking "),dl=[0,d("plugins/cc/ccalgo.ml"),651,2],de=d(aX),df=d(" <> "),dg=d(b6),dh=d(b7),di=d("Adding new disequality, depth="),c$=d(aX),da=d(" == "),db=d(b6),dc=d(b7),dd=d("Adding new equality, depth="),c_=d("discarding redundant (dis)equality"),c6=d(aX),c7=d(ca),c3=d(aX),c4=d(":="),c5=d(ca),c2=d("incomplete matching"),cR=d("not a node"),cS=[0,d("subterms")],cI=d("not a constructor"),cJ=[0,d("get_constructor")],cE=d("not a representative"),cF=[0,d("get_representative")],cu=d("signature already entered"),cv=[0,d("enter")],cl=[0,d(b8),[0,d("Verbose"),0]],cm=d("Congruence Verbose"),cO=d("Ccalgo.Discriminable"),cU=d(b3),cW=d(b3),dH=d("_eps_"),dR=d("invalid cc transitivity"),dS=d("not enough args"),dT=[0,d("nth_arg")],dU=[0,1,20],dV=d("equal_proof "),dW=[0,1,20],dX=d("edge_proof "),dY=[0,1,20],dZ=d("constr_proof "),d1=d(","),d0=d("}"),d2=d("{"),d3=[0,1,20],d4=d("path_proof "),d5=[0,1,20],d6=d("congr_proof "),d7=[0,1,20],d8=d("ind_proof "),eA=d("f"),eB=d("I don't know how to handle dependent equality"),eY=[0,0],eV=d("congruence failed."),eW=[0,d(b8)],eP=d("("),eQ=d(")"),eL=d("Goal solved, generating proof ..."),eK=d("Computation completed."),eJ=d("Problem built, solving ..."),eI=d("Reading subgoal ..."),eM=d("Goal is solvable by congruence but some arguments are missing."),eN=d("  replacing metavariables by arbitrary terms."),eO=d(')",'),eR=d('"congruence with ('),eS=d("  Try "),eT=d("Incomplete"),eU=d("congruence failed"),eG=d(b$),eH=d(ce),eF=d("H"),eD=d("e"),eE=d(b$),eC=d(ce),ey=[0,0],ez=[0,1],ew=d("t"),d9=d("CC"),d_=d(aQ),d$=[0,d(X),[0,d(W),0]],ea=d("eq_rect"),eb=[0,d(X),[0,d(W),0]],ec=d("eq_refl"),ed=[0,d(X),[0,d(W),0]],ee=d("eq_sym"),ef=[0,d(X),[0,d(W),0]],eh=d("eq_trans"),ei=[0,d(X),[0,d(W),0]],ej=d("eq"),ek=[0,d(X),[0,d(W),0]],el=d("False"),em=[0,d(X),[0,d(W),0]],en=d("True"),eo=[0,d(X),[0,d(W),0]],ep=d("I"),eq=[0,d(X),[0,d(W),0]],ft=d(aQ),fu=d(aQ),fo=[0,d(aV),1,0],fk=[0,d(aV),1,0],fh=[0,d(aV),1,0],fe=[0,d(aV),1,0],fd=d(cj),ff=[0,d(ch)],fg=d(cc),fi=[0,d(aW)],fj=d(cj),fl=[0,d(ch)],fm=[0,d(aW)],fn=d(cc),fp=[0,d(aW)],fq=[0,[0,d(aW)],0],fr=d(cd),e9=d(aU),e7=d(aU),e5=d(aU),e3=d(aU),e1=d(b4),fb=d(cd),fw=d(aQ),c1=i.Typeops,co=i.Goptions,er=i.Inductiveops,eZ=i.Type_errors,ev=i.Equality,eu=i.Evarutil,e$=i.Array,ad=5;function
x(d){var
c=aZ[1];if(c){var
e=a(d,0);return b(a0[10],0,e)}return c}function
ck(a){aZ[1]=a;return 0}var
cn=[0,1,0,cm,cl,function(a){return aZ[1]},ck];b(co[4],0,cn);var
cp=g[1],cq=[0,function(b,a){return b===a?1:0},cp],aA=a(ap[18],cq);function
cr(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
cs=[0,cr,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b(ak[2][1],f,e)}],aq=a(ap[18],cs);function
ct(f,e,d){if(b(aq[10],d[1],e)){var
g=a(c[3],cu);j(A[3],0,cv,g)}else
j(aq[9],d[1],e,f);return j(aA[9],d[2],f,e)}function
cw(c,a){return b(aq[7],a[1],c)}function
cx(a,c){try{var
d=b(aA[7],a[2],c);b(aq[6],a[1],d);var
e=b(aA[6],a[2],c);return e}catch(a){a=r(a);if(a===s)return 0;throw a}}function
cy(c,a){function
d(a){return cx(c,a)}return b(g[2][13],d,a)}var
cz=[0,function(b,a){var
c=ay(b[1],a[1]),e=a[3],f=a[2],g=b[3],i=b[2];if(0===c){var
d=ay(i,f);return 0===d?j(h[17][45],ay,g,e):d}return c}],cA=[0,function(b,a){var
c=ay(b[1],a[1]),d=a[2],e=b[2];return 0===c?ay(e,d):c}],o=a(h[21][1],cz),n=a(h[21][1],cA);function
bk(b,a){var
c=0===b[0]?0===b[1]?0===a[0]?0===a[1]?1:0:0:0===a[0]?0===a[1]?0:1:0:0===a[0]?0:1;return c?1:0}function
a1(n,m){var
c=n,a=m;for(;;){switch(c[0]){case
0:if(0===a[0])return b(e[cf],c[1],a[1]);break;case
1:if(1===a[0]){var
o=a[2],p=c[2],g=bk(c[1],a[1]);return g?bk(p,o):g}break;case
2:if(2===a[0])return b(k[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],h=a1(c[1],a[1]);if(h){var
c=r,a=q;continue}return h}break;default:if(4===a[0]){var
d=a[1],f=c[1],i=f[2]===d[2]?1:0,s=d[3],t=d[1][1],u=f[3],v=f[1][1];if(i){var
j=u===s?1:0;if(j)return b(k[46],v,t);var
l=j}else
var
l=i;return l}}return 0}}function
a2(c){switch(c[0]){case
0:var
f=a(e[b0],c[1]);return b(ak[2][1],1,f);case
1:var
g=c[1],h=a(bl[6],c[2]),i=a(bl[6],g);return j(ak[2][3],2,i,h);case
2:var
l=a(k[1][3],c[1]);return b(ak[2][1],3,l);case
3:var
m=c[1],n=a2(c[2]),o=a2(m);return j(ak[2][3],4,o,n);default:var
d=c[1],p=d[3],q=d[2],r=a(k[50],d[1][1]);return B(ak[2][4],5,r,q,p)}}var
I=a(ap[18],[0,e[cf],e[b0]]),ae=a(ap[18],[0,a1,a2]),a3=a(ap[18],[0,k[1][1],k[1][3]]),cB=[0,a(e[H],R[8])],bm=[0,[1,R[8],[0,R[8],R[8],0]],R[8],o[1],0,cB];function
bn(c){var
b=a(ae[1],ad);return[0,ad,0,aO(5,bm),a(I[1],ad),0,b]}function
cC(e,d){var
f=a(I[1],ad),h=a(a3[1],ad),i=g[2][1],j=a(t[2],0),k=a(t[2],0),l=g[2][1],b=a(aA[1],ad),c=[0,a(aq[1],ad),b];return[0,bn(0),c,l,k,j,0,0,i,h,e,0,f,d]}function
cD(a){return a[1]}function
v(e,g){var
c=0,a=g;for(;;){var
d=q(e[3],a)[a+1][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){q(e[3],b)[b+1][2]=a;return 0};b(h[17][11],f,c);return a}}function
S(e,b){var
d=q(e[3],b)[b+1][1];if(0===d[0])return d[1];var
f=a(c[3],cE);return j(A[3],0,cF,f)}function
a4(b,a){return q(b[3],a)[a+1][3]}function
cG(d,c,a){var
e=a4(d,c);return b(o[22],a,e)}function
cH(c,f,e){var
a=f;for(;;)try{var
g=a4(c,a),h=b(o[22],e,g);return h}catch(b){b=r(b);if(b===s){var
d=q(c[3],a)[a+1][1];if(0===d[0])throw s;var
a=d[1];continue}throw b}}function
bo(e,b){var
d=q(e[3],b)[b+1][5];if(4===d[0])return d[1];var
f=a(c[3],cI);return j(A[3],0,cJ,f)}function
bp(b,a){return S(b,a)[1]}function
cK(a){return a[4]}function
cL(a){return a[5]}function
cM(e,d,c){var
a=S(e,d);a[1]=a[1]+1|0;a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
cN(e,d,c){var
a=S(e,d);a[1]=a[1]+1|0;a[3]=b(g[2][4],c,a[3]);return 0}var
bq=[248,cO,w.caml_fresh_oo_id(0)];function
cP(b){var
c=a(h[17][4],b[3]);return[0,b[1],b[2]+1|0,c]}function
cQ(a,c,e){try{var
i=b(n[22],c,a[6]),d=i}catch(a){a=r(a);if(a!==s)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=j(n[4],c,h,f);return 0}function
al(b,a){return q(b[3],a)[a+1][5]}function
a5(f,b){var
d=q(f[3],b)[b+1][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cR);return j(A[3],0,cS,g)}function
br(a,c){var
b=a5(a,c),d=b[1],e=v(a,b[2]);return[0,v(a,d),e]}function
cT(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=aO(d,bm);a[1]=d;fx(h[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
aB(a){return[0,0,g[2][1],g[2][1],0,a,n[1]]}var
cV=[0,a(k[1][6],cU)],cX=[0,a(k[1][6],cW)],cY=a(e[H],2),cZ=[0,0,a(e[H],2),cY],c0=a(e[cg],cZ);function
bs(g,c){var
h=a(e[Q],g);if(10===h[0]){var
d=h[1],i=a(aC[2],0);if(b(ar[62],d[1],i)){var
f=b(k[C][1],d[1],0);if(c){var
j=c[2],l=a(e[bg],[0,f,c[1]]);return b(e[60],l,j)}var
m=a(aC[2],0),n=b(c1[27],m,d),o=a(aC[2],0),p=b(ar[61],f,o)[2]+1|0,q=b(e[85],p,n)[1],r=[0,f,a(e[H],1)],s=a(e[bg],r);return b(e[69],s,q)}}return b(e[60],g,c)}function
J(c){switch(c[0]){case
0:return bs(c[1],0);case
1:var
j=c[1],g=[0,cX,a(e[bj],c[2]),c0],h=a(e[aj],g),i=[0,cV,a(e[bj],j),h];return a(e[aj],i);case
2:return a(e[C],c[1]);case
3:var
k=c[1],f=[0,J(c[2]),0],d=k;for(;;){if(3===d[0]){var
l=d[1],f=[0,J(d[2]),f],d=l;continue}if(0===d[0])return bs(d[1],f);var
m=J(d);return b(e[60],m,f)}default:return a(e[be],c[1][1])}}function
E(d){var
c=a(e[Q],d);switch(c[0]){case
6:var
n=c[2],o=c[1],p=E(c[3]),q=[0,o,E(n),p];return a(e[cg],q);case
7:var
r=c[2],s=c[1],t=E(c[3]),u=[0,s,E(r),t];return a(e[aj],u);case
8:var
v=c[3],w=c[2],x=c[1],y=E(c[4]),z=E(v),A=[0,x,E(w),z,y];return a(e[119],A);case
9:var
B=c[1],F=b(h[19][51],E,c[2]),G=[0,E(B),F];return a(e[D],G);case
10:var
f=c[1],H=f[2],I=a(k[aY],f[1]),J=[0,a(k[aR],I),H];return a(e[bi],J);case
11:var
g=c[1],i=g[1],K=g[2],L=i[2],M=a(k[aS],i[1]),N=[0,[0,a(k[ao],M),L],K];return a(e[ao],N);case
12:var
j=c[1],l=j[1],m=l[1],O=j[2],P=l[2],R=m[2],S=a(k[aS],m[1]),T=[0,[0,[0,a(k[ao],S),R],P],O];return a(e[be],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(k[aY],b);return a(k[aR],c)},X=b(k[C][10],W,V),Y=[0,X,E(U)];return a(e[bg],Y);default:return d}}function
a6(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,a6(b,c)]};return j(h[17][16],f,d,e)}var
c=a[1]-1|0;return q(b,c)[c+1]}function
T(e,d){var
f=a(c[3],c3),g=J(al(e,d)),h=a(K[5],g),i=a(c[3],c4),j=a(c[16],d),k=a(c[3],c5),l=b(c[12],k,j),m=b(c[12],l,i),n=b(c[12],m,h);return b(c[12],n,f)}function
aD(d){var
e=a(c[3],c6),f=J(d),g=a(K[5],f),h=a(c[3],c7),i=b(c[12],h,g);return b(c[12],i,e)}function
Y(d,c){var
e=d[1];try{var
h=b(ae[7],e[6],c);return h}catch(h){h=r(h);if(h===s){var
a=cT(e),p=J(c),f=E(b(l[15],d[13],p));switch(c[0]){case
2:var
y=o[1],i=[0,[0,aB(f)],-1,y,0,c];break;case
3:var
z=c[2],m=Y(d,c[1]),n=Y(d,z);cM(e,v(e,m),a);cN(e,v(e,n),a);d[3]=b(g[2][4],a,d[3]);var
A=o[1],i=[0,[0,aB(f)],-1,A,[0,[0,m,n]],c];break;case
4:var
B=c[1];b(t[3],[0,a,[0,[0,a,0]]],d[5]);b(t[3],[0,a,[1,[0,a,B[2],0]]],d[5]);var
C=o[1],i=[0,[0,aB(f)],-1,C,0,c];break;default:b(t[3],[0,a,[0,[0,a,0]]],d[5]);var
u=o[1],i=[0,[0,aB(f)],-1,u,0,c]}q(e[3],a)[a+1]=i;j(ae[5],e[6],c,a);try{var
x=b(I[7],d[12],f),k=x}catch(a){a=r(a);if(a!==s)throw a;var
k=g[2][1]}var
w=b(g[2][4],a,k);j(I[9],d[12],f,w);return a}throw h}}function
bt(a,e,d,c){var
f=Y(a,d),g=Y(a,c);b(t[3],[0,f,g,[0,e,0]],a[4]);return j(I[5],a[1][4],e,[0,d,c])}function
bu(a,d,c,b){var
e=Y(a,c),f=Y(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
c8(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
c9(a,d,c){try{var
e=a[1],f=function(a){return v(e,a)},g=b(h[19][15],f,c),i=b(a3[8],a[9],d),k=function(b){function
c(c,b){return c===v(a[1],b)?1:0}return j(h[19][31],c,g,b)},l=b(h[17][23],k,i);return l}catch(a){a=r(a);if(a===s)return 0;throw a}}function
dj(e,b,a,d){var
c=q(e[3],b)[b+1];c[1]=[1,a,d];c[2]=a;return 0}function
bv(g,f,e){var
a=f,b=e;for(;;){var
c=q(g[3],a)[a+1][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
dk(c,i,h){var
o=v(c,h);if(v(c,i)===o){var
p=bv(c,h,0),a=[0,bv(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,as,dl]}function
bw(d,h,k,w){x(function(o){var
e=a(c[3],dm),f=T(d[1],k),g=a(c[3],dn),i=T(d[1],h),j=a(c[3],dp),l=b(c[12],j,i),m=b(c[12],l,g),n=b(c[12],m,f);return b(c[12],n,e)});var
i=S(d[1],h),e=S(d[1],k);dj(d[1],h,k,w);try{var
G=b(I[7],d[12],i[5]),p=G}catch(a){a=r(a);if(a!==s)throw a;var
p=g[2][1]}var
y=b(g[2][6],h,p);j(I[9],d[12],i[5],y);var
u=b(g[2][7],i[3],e[3]);e[1]=a(g[2][20],u);e[3]=u;e[2]=b(g[2][7],i[2],e[2]);cy(d[2],i[3]);d[3]=b(g[2][7],d[3],i[3]);var
z=q(d[1][3],h)[h+1][3];function
A(c,a){return b(t[3],[0,a,[1,c]],d[5])}b(o[10],A,z);var
B=i[6];function
C(c){function
e(a){return b(t[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(n[10],C,B);var
l=i[4],f=e[4];if(typeof
l==="number"){if(0===l)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],k,d[8]);e[4]=1;return 0}}else
if(0===l[0]){var
D=l[1];if(typeof
f==="number"){if(0===f){e[4]=[0,D];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],k,d[8]);return 0}var
v=0}else
var
v=1===f[0]?1:0;if(!v){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
m=l[1],E=m[2],F=m[1];if(typeof
f==="number"){if(0===f){e[4]=[1,m];return 0}}else
if(0!==f[0])return b(t[3],[0,F,[1,E]],d[5])}return 0}function
dq(e,f){x(function(n){var
d=a(c[3],dr),g=T(f[1],e[2]),h=a(c[3],ds),i=T(f[1],e[1]),j=a(c[3],dt),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=f[1],h=v(g,e[1]),i=v(g,e[2]),j=1-(h===i?1:0);if(j){var
l=bp(g,i);if(bp(g,h)<l)return bw(f,h,i,e);var
d=e[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bw(f,i,h,[0,e[2],e[1],k])}return j}function
dy(f,s,d){x(function(j){var
e=a(c[3],dz),g=T(d[1],f),h=a(c[3],dA),i=b(c[12],h,g);return b(c[12],i,e)});var
p=v(d[1],f),h=S(d[1],p);if(0===s[0]){cQ(h,s[1],f);d[3]=b(g[2][7],h[2],d[3]);return 0}var
e=s[1],r=q(d[1][3],p)[p+1];if(1-b(o[3],e,r[3]))r[3]=j(o[4],e,f,r[3]);var
i=h[4];if(typeof
i==="number"){if(0===i)return 0===e[2]?(h[4]=[1,[0,f,e]],0):(d[3]=b(g[2][7],h[2],d[3]),h[4]=[0,e],d[8]=b(g[2][4],p,d[8]),0)}else
if(1===i[0]){var
u=i[1],k=u[2],w=u[1];if(e[1]===k[1]){var
z=bo(d[1],e[1]),n=z[3],m=k[3],l=e[3];for(;;){var
y=0<n?1:0;if(y){if(m)if(l){var
B=l[2],C=m[2];b(t[3],[0,m[1],l[1],[1,w,k,f,e,n]],d[4]);var
n=n-1|0,m=C,l=B;continue}var
D=a(c[3],dw);return j(A[3],0,dx,D)}return y}}throw[0,bq,w,k,f,e]}d[3]=b(g[2][7],h[2],d[3]);return 0}function
dB(e){var
g=e[1];function
h(f){if(f){var
d=f[1],k=f[2],l=v(g,d[2]);if(v(g,d[1])===l)var
j=a(c[3],dC),i=[0,d];else
var
m=h(k),j=a(c[3],dG),i=m;x(function(p){var
f=a(c[3],dD),g=T(e[1],d[2]),h=a(c[3],dE),i=T(e[1],d[1]),k=a(c[3],dF),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(e[6])}var
dI=a(k[1][6],dH);function
dK(d){var
f=d[8];function
i(q){var
i=S(d[1],q)[4];if(typeof
i!=="number"&&0===i[0]){var
f=i[1],v=J(al(d[1],f[1])),w=b(l[15],d[13],v),x=f[3],y=function(a){return J(al(d[1],a))},z=b(h[17][12],y,x),B=a(h[17][6],z),D=b(e[76],w,B),E=f[2],m=al(d[1],q),o=D,k=E;for(;;){if(0<k){var
p=a(e[34],o),s=p[3],t=p[2],g=b(l[20],dI,d[13]),n=d[13];d[13]=j(bx[4][12],n[2],n[1],[0,[0,g,t],0]);var
u=[0,a(e[C],g),0],m=[3,m,[2,g]],o=b(by[13],u,s),k=k-1|0;continue}d[1][5]=[0,f,d[1][5]];Y(d,m);return 0}}var
r=a(c[3],dJ);return j(A[3],0,0,r)}return b(g[2][13],i,f)}function
bz(c){var
a=[0,n[1]],f=c[1],d=c[1][3];function
e(c,h){var
d=c<f[2]?1:0;if(d){var
e=h[1];if(0===e[0]){var
i=e[1][6],k=function(d,k){try{var
i=b(n[22],d,a[1]),e=i}catch(a){a=r(a);if(a!==s)throw a;var
e=g[2][1]}var
f=a[1],h=b(g[2][4],c,e);a[1]=j(n[4],d,h,f);return 0};return b(n[10],k,i)}return 0}return d}b(h[19][14],e,d);return a[1]}function
bA(t,p,d){var
c=a(h[22][9],d),l=c[3];if(l){var
e=l[2],u=l[1],f=u[2],i=u[1],j=t[1];if(0===i[0]){var
k=i[2],m=i[1];if(k){var
A=k[2],B=k[1];try{var
C=b(ae[7],j[6],m),D=[0,C,a(h[17][1],k)],E=S(j,f)[6],F=b(n[22],D,E),G=function(g){var
f=br(t[1],g),i=[0,[0,[0,m,A],f[1]],[0,[0,B,f[2]],e]],j=c[2],k=[0,a(h[19][8],c[1]),j,i];return b(h[22][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=r(a);if(a===s)return 0;throw a}}try{var
w=v(j,b(ae[7],j[6],m))===f?1:0,I=w?b(h[22][3],[0,c[1],c[2],e],d):w;return I}catch(a){a=r(a);if(a===s)return 0;throw a}}var
o=i[1],x=o-1|0;if(0<=q(c[1],x)[x+1]){var
y=o-1|0;return q(c[1],y)[y+1]===f?b(h[22][3],[0,c[1],c[2],e],d):0}var
z=o-1|0;q(c[1],z)[z+1]=f;return b(h[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
a8(d,c){if(0===c[0]){var
e=c[1],f=a(h[17][1],c[2]);return[0,b(ae[7],d,e),f]}return b(A[10],0,dL)}function
bB(c){var
k=c[1][6],f=a(h[22][2],0),l=bz(c);function
d(a){var
i=a[5];if(typeof
i==="number")if(0===i)try{var
x=a8(k,a[4]),y=b(n[22],x,l),d=y}catch(a){a=r(a);if(a!==s)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=i[1];try{var
A=b(I[7],c[12],z),o=A}catch(a){a=r(a);if(a!==s)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(h[22][3],[0,aO(a[3],-1),a,[0,[0,a[4],c],0]],f)}b(g[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
t=a8(k,a[6]),u=b(n[22],t,l),e=u}catch(a){a=r(a);if(a!==s)throw a;var
e=g[2][1]}else
var
e=g[2][1];else{var
v=j[1];try{var
w=b(I[7],c[12],v),m=w}catch(a){a=r(a);if(a!==s)throw a;var
m=g[2][1]}var
e=m}function
q(c){return b(h[22][3],[0,aO(a[3],-1),a,[0,[0,a[6],c],0]],f)}return b(g[2][13],q,e)}b(h[17][11],d,c[7]);return f}function
bC(b){var
d=[0,0],e=bB(b);x(function(b){return a(c[3],dM)});try{for(;;){a(a7[2],0);bA(b,d,e);continue}}catch(a){a=r(a);if(a===h[22][1])return d[1];throw a}}function
a9(v,d){x(function(b){return a(c[3],dN)});try{for(;;){a(a7[2],0);try{dq(a(t[5],d[4]),d);var
L=1,i=L}catch(e){e=r(e);if(e!==t[1])throw e;try{var
u=a(t[5],d[5]);dy(u[1],u[2],d);var
I=1,i=I}catch(e){e=r(e);if(e!==t[1])throw e;try{var
f=a(g[2][24],d[3]);d[3]=b(g[2][6],f,d[3]);x(function(i){return function(j){var
e=a(c[3],du),f=T(d[1],i),g=a(c[3],dv),h=b(c[12],g,f);return b(c[12],h,e)}}(f));var
l=br(d[1],f),m=l[1],y=a5(d[1],f)[2],p=S(d[1],m),q=p[4],Y=typeof
q==="number"?0:0===q[0]?(p[4]=1,d[8]=b(g[2][6],m,d[8]),1):0,z=a4(d[1],m),B=function(c,e){return function(a,f){return b(t[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(y,f);b(o[10],B,z);var
E=p[6],F=function(c){return function(a,e){return b(t[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(f);b(n[10],F,E);try{var
G=cw(l,d[2]);b(t[3],[0,f,G,0],d[4])}catch(a){a=r(a);if(a!==s)throw a;ct(f,l,d[2])}var
H=1,i=H}catch(a){a=r(a);if(a!==s)throw a;var
i=0}}}if(i)continue;var
w=dB(d);if(w)var
R=w[1],U=v?[1,R]:0,k=[0,U];else
if(a(g[2][2],d[8]))if(0<d[10]){var
V=bC(d),W=function(p){var
m=p[2],f=p[1];a(a7[2],0);var
n=0<d[10]?1:0;if(n){if(c9(d,f[1],m))return x(function(b){return a(c[3],c_)});j(a3[5],d[9],f[1],m);var
s=d[1],q=function(b){try{var
e=al(s,b);return e}catch(b){b=r(b);if(a(A[21],b)){var
d=a(c[3],c2);return j(A[3],0,0,d)}throw b}},l=b(h[19][15],q,m),t=a(e[C],f[1]),o=b(h[19][15],J,l);a(h[19][40],o);var
g=a(e[D],[0,t,o]),i=a6(l,f[4]),k=a6(l,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(x(function(z){var
e=a(c[3],c$),f=aD(k),h=a(c[3],da),j=aD(i),l=a(c[3],db),m=a(K[5],g),n=a(c[3],dc),o=b(c[12],n,m),p=b(c[12],o,l),q=b(c[12],p,j),r=b(c[12],q,h),s=b(c[12],r,f),t=b(c[12],s,e),u=a(c[5],0),v=a(c[16],d[10]),w=a(c[3],dd),x=b(c[12],w,v),y=b(c[12],x,u);return b(c[12],y,t)}),bt(d,g,i,k)):(x(function(z){var
e=a(c[3],de),f=aD(k),h=a(c[3],df),j=aD(i),l=a(c[3],dg),m=a(K[5],g),n=a(c[3],dh),o=b(c[12],n,m),p=b(c[12],o,l),q=b(c[12],p,j),r=b(c[12],q,h),s=b(c[12],r,f),t=b(c[12],s,e),u=a(c[5],0),v=a(c[16],d[10]),w=a(c[3],di),x=b(c[12],w,v),y=b(c[12],x,u);return b(c[12],y,t)}),bu(d,[0,g],i,k))}return n};b(h[17][11],W,V);var
X=d[11]?(d[11]=0,a9(1,d)):(x(function(b){return a(c[3],dO)}),0),k=X}else{x(function(b){return a(c[3],dP)});var
k=0}else{x(function(b){return a(c[3],dQ)});dK(d);var
k=a9(0,d)}return k}}catch(a){a=r(a);if(a[1]===bq){var
M=a[5],N=a[4],O=a[3],P=a[2],Q=v?[0,[0,P,O,N,M]]:0;return[0,Q]}throw a}}var
f=[0,[0,n[1],n[2],n[3],n[4],n[5],n[6],n[7],n[8],n[9],n[10],n[11],n[12],n[13],n[14],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24]],[0,o[1],o[2],o[3],o[4],o[5],o[6],o[7],o[8],o[9],o[10],o[11],o[12],o[13],o[14],o[15],o[16],o[17],o[18],o[19],o[20],o[21],o[22],o[23],o[24]],I,ae,a1,J,x,cD,cK,cL,cC,Y,bt,bu,c8,cP,v,cG,cH,al,bo,a5,dk,bz,bA,bB,a8,bC,a9,T,bn];az(169,f,"Cc_plugin.Ccalgo");function
at(a){return[0,a,a,[2,a]]}function
am(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return at([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
y(o,n){var
e=o,d=n;for(;;){var
g=e[3],h=d[3];switch(g[0]){case
2:return d;case
4:var
l=g[2],m=g[1];switch(h[0]){case
2:var
i=0;break;case
3:var
k=h[1][3];if(4===k[0]){var
r=h[2],s=k[1],t=y(l,k[2]),e=am(y(m,s),t),d=r;continue}var
i=1;break;case
4:var
u=h[1],v=y(l,h[2]);return am(y(m,u),v);default:var
i=1}break;default:var
i=0}if(!i){if(2===h[0])return e;if(3===g[0]){var
q=g[1],e=q,d=y(g[2],d);continue}}if(b(f[5],e[2],d[1]))return[0,e[1],d[2],[3,e,d]];var
p=a(c[3],dR);return j(A[3],0,0,p)}}function
U(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=U(a[1]);return y(U(c),d);case
4:var
e=a[1],f=U(a[2]);return am(U(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,U(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bD(d,a){var
c=b(f[3][7],d,a);return[0,c[1],c[2],[0,a]]}function
bE(d,a){var
c=b(f[3][7],d,a);return[0,c[2],c[1],[1,a]]}function
bF(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],i=b[1];if(0<d){var
b=i,d=d-1|0;continue}return h}var
g=a(c[3],dS);return j(A[3],0,dT,g)}}function
bG(c,d,b,a){var
e=bF(c[2],b-a|0);return[0,bF(c[1],b-a|0),e,[5,c,d,b,a]]}function
Z(d,e,g){function
i(n){var
h=b(f[30],d,g),i=a(c[4],dU),j=b(f[30],d,e),k=a(c[3],dV),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],i);if(e===g)return at(b(f[20],d,e));var
h=j(f[23],d,e,g),k=h[1],l=U(aE(d,g,h[2]));return y(aE(d,e,k),l)}function
bH(d,i){var
g=i[2],j=i[1],k=j[2],l=j[1];function
p(n){var
e=b(f[30],d,k),g=a(c[4],dW),h=b(f[30],d,l),i=a(c[3],dX),j=b(c[12],i,h),m=b(c[12],j,g);return b(c[12],m,e)}a(f[7],p);var
q=Z(d,l,g[1]),r=U(Z(d,k,g[2])),e=g[3];if(typeof
e==="number")var
h=bI(d,g[1],g[2]);else
if(0===e[0])var
m=e[1],s=e[2]?bE(a(f[9],d),m):bD(a(f[9],d),m),h=s;else
var
n=e[2],t=e[5],u=a$(d,e[1],n,e[3],e[4]),o=b(f[21],d,n[1]),h=bG(u,o[1],o[3],t);return y(y(q,h),r)}function
a_(d,g,e){function
l(k){var
e=a(c[4],dY),h=b(f[30],d,g),i=a(c[3],dZ),j=b(c[12],i,h);return b(c[12],j,e)}a(f[7],l);var
h=j(f[19],d,g,e),i=Z(d,g,h);if(0===e[3])return i;var
m=a(f[16],e),k=b(f[22],d,h),n=k[1],o=b(f[20],d,k[2]),p=a_(d,n,m);return y(i,am(p,at(o)))}function
aE(e,g,d){function
i(u){var
h=a(c[3],d0);function
i(b){return a(c[16],b[1][2])}function
k(b){return a(c[3],d1)}var
l=j(c[38],k,i,d),m=a(c[3],d2),n=a(c[4],d3),o=b(f[30],e,g),p=a(c[3],d4),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m),t=b(c[12],s,l);return b(c[12],t,h)}a(f[7],i);if(d){var
h=d[1],k=d[2],l=bH(e,h);return y(aE(e,h[1][2],k),l)}return at(b(f[20],e,g))}function
bI(d,g,e){function
j(n){var
h=b(f[30],d,e),i=a(c[4],d5),j=b(f[30],d,g),k=a(c[3],d6),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
h=b(f[22],d,g),k=h[2],l=h[1],i=b(f[22],d,e),m=i[1],n=Z(d,k,i[2]);return am(Z(d,l,m),n)}function
a$(d,g,i,e,h){function
j(n){var
h=b(f[30],d,e),i=a(c[4],d7),j=b(f[30],d,g),k=a(c[3],d8),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
k=Z(d,g,e),l=a_(d,g,i),m=y(k,a_(d,e,h));return y(U(l),m)}var
aF=[0,at,am,y,U,bD,bE,bG,Z,bH,aE,bI,a$,function(c,b){if(b1<=b[1]){var
a=b[2];return a$(c,a[1],a[2],a[3],a[4])}var
d=b[2];return Z(c,d[1],d[2])}];az(170,aF,"Cc_plugin.Ccproof");function
V(b,a){return[G,function(c){return j(ba[5],d9,b,a)}]}var
aG=V(d$,d_),bJ=V(eb,ea),bK=V(ed,ec),eg=V(ef,ee),bL=V(ei,eh),u=V(ek,ej),_=V(em,el),aH=V(eo,en),aI=V(eq,ep);function
bM(c){var
d=j($[42],0,$[14],c);return function(c){var
e=a($[36],c);return b($[47],d,e)}}function
aJ(c){var
d=j($[42],0,$[9],c);return function(c){var
e=a($[36],c);return b($[47],d,e)}}function
aK(c,b,a){return j(au[4],c,[0,b],a)}function
z(d,f,i){var
y=a(bM(d),i),c=a(e[Q],y);switch(c[0]){case
6:var
m=c[3],n=c[2],A=a(e[H],1);if(!b(K[45],A,m)){var
o=a(K[56],m),B=aK(d,f,o),D=aK(d,f,n),E=z(d,f,o);return[3,[3,[1,D,B],z(d,f,n)],E]}break;case
9:var
F=c[2],G=z(d,f,c[1]),I=function(a){return z(d,f,a)},J=b(h[19][15],I,F),L=function(b,a){return[3,b,a]};return j(h[19][17],L,G,J);case
10:var
p=c[1],M=p[2],N=a(k[aY],p[1]),O=[0,a(k[aR],N),M];return[0,a(e[bi],O)];case
11:var
q=c[1],r=q[1],P=q[2],R=r[2],S=a(k[aS],r[1]),T=[0,[0,a(k[ao],S),R],P];return[0,a(e[ao],T)];case
12:var
t=c[1],u=t[1],v=u[2],w=u[1],U=t[2],V=w[2],W=a(k[aS],w[1]),g=[0,a(k[ao],W),V],X=a(aC[26],g)[1],x=b(er[44],d,[0,g,v]);return[4,[0,[0,[0,g,v],U],x,x-X[6]|0]];case
16:var
Y=c[2],Z=c[1],_=function(b){var
c=a(k[aY],b);return a(k[aR],c)},$=b(k[C][10],_,Z),aa=z(d,f,Y),ab=a(k[C][3],$);return[3,[0,a(e[121],ab)],aa]}var
l=a(K[67],i);if(a(by[2],l))return[0,l];throw s}function
bb(c,d,g){var
j=a(aJ(c),g),h=a(e[Q],j);if(9===h[0]){var
f=h[2],i=O(u),k=h[1],l=P===i?u[1]:G===i?a(L[2],u):u;if(b(av[11],l,k))if(3===f.length-1){var
m=z(c,d,q(f,2)[3]),n=z(c,d,q(f,1)[2]);return[0,aP,[0,q(f,0)[1],n,m]]}return[0,bh,z(c,d,g)]}return[0,bh,z(c,d,g)]}function
aw(c,d,i){var
r=a(bM(c),i),f=a(e[Q],r);switch(f[0]){case
0:var
k=f[1];return[0,[1,k],a(g[2][5],k)];case
6:var
l=f[3],m=f[2],t=a(e[H],1);if(!b(K[45],t,l)){var
n=a(K[56],l),o=aw(c,d,m),u=o[2],v=o[1],p=aw(c,d,n),w=p[2],x=p[1],y=aK(c,d,n),A=aK(c,d,m);return[0,[0,[1,A,y],[0,v,[0,x,0]]],b(g[2][7],u,w)]}break;case
9:var
B=f[2],C=z(c,d,f[1]),D=function(a){return aw(c,d,a)},E=b(h[19][48],D,B),q=a(h[17][38],E),F=q[1],G=j(h[17][15],g[2][7],g[2][1],q[2]);return[0,[0,C,a(h[17][6],F)],G]}var
s=z(c,d,i);return[0,[0,s,0],g[2][1]]}function
bN(a){return 0===a[0]?1:0}function
bO(f,h,d,t){try{var
v=a(aJ(f),t),i=a(e[37],v)}catch(a){a=r(a);if(a===e[28])throw s;throw a}var
c=i[2],j=O(u),w=i[1],x=P===j?u[1]:G===j?a(L[2],u):u;if(b(av[11],x,w))if(3===c.length-1){var
k=aw(f,h,q(c,1)[2]),l=k[1],y=k[2],m=aw(f,h,q(c,2)[3]),n=m[1],z=m[2],o=a(g[2][20],y)===d?bN(l)?0:[0,q(c,0)[1]]:1,p=a(g[2][20],z)===d?bN(n)?0:[0,q(c,0)[1]]:1;if(1===o)if(1===p)throw s;return[0,d,o,l,p,n]}throw s}function
es(n,h,m,l){var
c=n,d=m,g=l;for(;;){var
o=a(aJ(c),g),f=a(e[Q],o);if(6===f[0]){var
i=f[3],j=f[2],k=O(_),p=f[1],q=P===k?_[1]:G===k?a(L[2],_):_;if(b(av[11],q,i))return[0,b_,bO(c,h,d,j)];var
c=b(ar[20],[0,p,j],c),d=d+1|0,g=i;continue}return[0,b2,bO(c,h,d,g)]}}function
et(c,d,g){var
m=a(aJ(c),g),f=a(e[Q],m);if(6===f[0]){var
j=f[3],k=f[2],l=O(_),n=f[1],o=P===l?_[1]:G===l?a(L[2],_):_;if(b(av[11],o,j)){var
h=bb(c,d,k);if(aP<=h[1]){var
i=h[2];return[0,b5,[0,i[1],i[2],i[3]]]}return[0,ci,h[2]]}try{var
p=es(b(ar[20],[0,n,k],c),d,1,j);return p}catch(a){a=r(a);if(a===s)return[0,bh,z(c,d,g)];throw a}}return bb(c,d,g)}function
bQ(d,h,g,f,c){var
i=h[1][2],j=a(e[H],1),m=a(l[2],c),n=a(l[8],c),o=bd(ev[38],n,m,i,j,d,g,f),p=a(k[1][6],ew),q=[0,[0,b(l[20],p,c)],d,o];return a(e[aj],q)}var
aa=e[110];function
af(c,g,f){var
d=O(c);function
h(b){return a(f,a(e[D],[0,b,g]))}var
i=P===d?c[1]:G===d?a(L[2],c):c;return b(p[67],i,h)}function
ag(c,g,f){var
d=O(c);function
h(b){return a(f,a(e[D],[0,b,g]))}var
i=P===d?c[1]:G===d?a(L[2],c):c;return b(p[70][58],i,h)}function
ex(b){var
c=a(l[45],b);return a(m[67][1],c)}function
aM(d,c){var
e=[0,function(e){function
f(a){return b(au[2],0,a)}var
g=j(l[48][1],f,e,c)[1],h=b(F[138],d,c),i=a(aN[11],g),k=a(m[67][1],i);return b(p[70][3],k,h)}];return a(m[63][10],e)}function
bR(c,b,a){return bd(bS[4],[0,aL[cb]],0,ez,ey,c,b,a)}function
M(f,e){var
c=[0,function(c){var
g=a(m[63][5],c),d=bR(g,a(l[48][4],c),f),h=d[1],i=a(e,d[2]),j=a(aN[11],h),k=a(m[67][1],j);return b(p[70][3],k,i)}];return a(m[63][10],c)}function
N(j){var
d=[0,function(q){function
g(a){return b(l[48][7],q,a)}try{var
d=j[3];switch(d[0]){case
0:var
h=a(F[45],d[1]);break;case
1:var
E=d[1],w=a(f[6],j[1]),G=a(f[6],j[2]),I=function(a){return ag(eg,[0,a,G,w,E],F[45])},h=M(g(w),I);break;case
2:var
x=d[1],J=a(f[6],x),K=function(b){var
c=F[45];return ag(bK,[0,b,a(f[6],x)],c)},h=M(g(J),K);break;case
3:var
y=d[2],s=d[1],L=a(f[6],s[1]),z=a(f[6],s[2]),O=a(f[6],y[2]),P=function(c){var
d=a(aa,2),e=[0,c,L,z,O,a(aa,1),d],f=[0,N(y),0],g=[0,N(s),f],h=ag(bL,e,ex);return b(p[70][19],h,g)},h=M(g(z),P);break;case
4:var
t=d[2],u=d[1],n=a(f[6],u[1]),i=a(f[6],t[1]),o=a(f[6],u[2]),A=a(f[6],t[2]),Q=function(f){function
d(h){function
d(d){function
g(c){var
d=a(k[1][6],eA);return b(l[20],d,c)}var
j=b(l[48][3],g,q),r=[0,a(e[H],1),[0,i]],s=[0,[0,j],f,a(e[D],r)],v=a(e[aj],s),w=[0,f,d,v,n,o,a(aa,1)],x=[0,h,d,o,i,A,a(aa,1)],y=a(aa,3),z=a(aa,2),B=a(e[D],[0,o,[0,A]]),C=a(e[D],[0,o,[0,i]]),E=[0,d,a(e[D],[0,n,[0,i]]),C,B,z,y],G=a(c[3],eB),I=[0,b(p[70][5],0,G),0],J=[0,F[124],I],K=N(t),U=af(aG,x,l[45]),L=a(m[67][1],U),M=[0,b(p[70][3],L,K),J],O=[0,a(p[70][24],M),0],P=N(u),V=af(aG,w,l[45]),Q=a(m[67][1],V),R=[0,b(p[70][3],Q,P),O],T=af(bL,E,l[45]),S=a(m[67][1],T);return b(p[70][19],S,R)}return M(g(a(e[D],[0,n,[0,i]])),d)}return M(g(i),d)},h=M(g(n),Q);break;default:var
v=d[1],R=d[4],S=d[3],T=d[2],B=a(f[6],v[1]),U=a(f[6],v[2]),C=a(f[6],j[1]),V=a(e[H],(1+S|0)-R|0),W=function(c){function
d(d){function
e(a){return bQ(c,T,V,C,a)}var
f=b(l[48][3],e,q),g=[0,c,d,f,B,U,a(aa,1)],h=N(v),j=af(aG,g,l[45]),i=a(m[67][1],j);return b(p[70][3],i,h)}return M(g(C),d)},h=M(g(B),W)}return h}catch(c){c=r(c);if(a(m[67][10],c))return b(m[18],0,c);throw c}}];return a(m[63][9],d)}function
bT(d,c){function
e(a){return b(au[2],0,a)}var
f=j(l[23],e,c,d)[1],g=a(F[45],d),h=a(m[67][8],g),i=a(aN[11],f);return j(p[5],i,h,c)}function
bU(n,j,h,i){var
c=[0,function(c){var
g=a(f[6],j),d=a(f[6],h);function
o(f){var
o=a(k[1][6],eD),q=a(l[20],o),h=b(l[48][3],q,c),r=a(k[1][6],eE),s=a(l[20],r),t=b(l[48][3],s,c),v=[0,[0,t],f,a(e[H],1)],w=a(e[aj],v),A=af(bJ,[0,f,g,w,n,d,a(e[C],h)],bT),x=[0,a(m[67][1],A),0],j=[0,f,g,d],y=[0,N(i),x],z=[0,h],B=ag(u,j,function(a){return aM(z,a)});return b(p[70][19],B,y)}return M(b(l[48][7],c,d),o)}];return a(m[63][9],c)}function
bW(R,Q){var
d=[0,function(v){a(ba[11],ba[14]);function
S(b){return a(c[3],eI)}a(f[7],S);function
T(d){var
m=a(l[8],d),j=a(aL[69],d),c=b(f[11],R,d),k=[0,0],n=[0,0];function
o(a){var
d=z(m,j,a);b(f[12],c,d);return 0}b(h[17][11],o,Q);var
p=b(bx[4][3],d[2],d[1]),q=a(ar[26],p);function
r(o){var
l=a(bP[2][1][1],o),g=a(e[C],l),d=et(m,j,a(bP[2][1][3],o)),i=d[1];if(aP<=i){if(b_<=i)return b2<=i?B(f[15],c,l,1,d[2]):B(f[15],c,l,0,d[2]);if(b5<=i){var
p=d[2];return B(f[14],c,[0,g],p[2],p[3])}var
q=d[2];return B(f[13],c,g,q[2],q[3])}if(ci<=i){var
r=d[2],t=k[1],u=function(a){return B(f[14],c,[2,a[1],g],a[2],r)};b(h[17][11],u,t);n[1]=[0,[0,g,r],n[1]];return 0}var
s=d[2],v=n[1];function
w(a){return B(f[14],c,[2,g,a[1]],s,a[2])}b(h[17][11],w,v);k[1]=[0,[0,g,s],k[1]];return 0}b(h[17][11],r,q);var
s=a(l[7],d),g=bb(m,j,b(eu[32],j,s));if(aP<=g[1]){var
i=g[2];B(f[14],c,0,i[2],i[3])}else{var
t=g[2],u=k[1],v=function(a){return B(f[14],c,[1,a[1]],a[2],t)};b(h[17][11],v,u)}return c}var
w=b(l[48][3],T,v);function
U(b){return a(c[3],eJ)}a(f[7],U);var
x=b(f[29],1,w);function
V(b){return a(c[3],eK)}a(f[7],V);var
d=a(f[8],w);if(x){var
n=x[1],W=function(b){return a(c[3],eL)};a(f[7],W);if(typeof
n==="number"){var
y=[0,0],X=a(m[63][5],v),Y=function(b){y[1]++;return a(aa,y[1])},Z=a(f[10],d),_=function(c){var
g=b(f[21],d,c[1]),i=c[3];function
j(c){var
e=b(f[20],d,c);return a(f[6],e)}var
k=b(h[17][12],j,i),l=b(h[17][48],c[2],Y),m=a(h[17][6],l),n=b(h[17][8],k,m),o=a(e[be],g[1]);return b(e[60],o,n)},$=b(h[17][12],_,Z),ab=a(c[3],eM);b(a0[6],0,ab);var
ac=a(c[3],eN),ad=a(c[3],eO),ae=a(K[6],X),ah=function(h){var
d=a(c[3],eP),e=a(c[13],0),f=a(c[3],eQ),g=b(c[12],f,e);return b(c[12],g,d)},ai=j(c[38],ah,ae,$),ak=a(c[3],eR),al=b(c[12],ak,ai),am=b(c[12],al,ad),an=b(c[26],8,am),ao=a(c[3],eS),ap=b(c[12],ao,an),aq=b(c[12],ap,ac);b(a0[6],0,aq);var
as=a(c[3],eT);return b(p[70][4],0,as)}else{if(0===n[0]){var
o=n[1],A=o[2],s=b(aF[13],d,[0,b1,[0,o[1],A,o[3],o[4]]]),at=b(f[21],d,A[1])[1],J=[0,function(c){var
d=a(f[6],s[1]),i=a(f[6],s[2]),w=a(m[63][5],c),j=a(m[63][3],c),x=a(k[1][6],eG),y=a(l[20],x),n=O(aI),z=b(l[48][3],y,c),A=P===n?aI[1]:G===n?a(L[2],aI):aI,o=O(aH),D=a(bV[46],A),E=P===o?aH[1]:G===o?a(L[2],aH):aH,q=a(bV[46],E),F=a(l[48][4],c),r=bR(w,F,b(l[48][7],c,d)),g=r[2],t=B(aL[bi],0,0,aL[cb],r[1]),I=t[1],h=a(e[bj],t[2]),J=[0,[0,z],h,a(e[H],1)],K=a(e[aj],J),M=a(k[1][6],eH),Q=a(l[20],M),v=b(l[48][3],Q,c);function
R(a){return bQ(g,at,q,j,a)}var
S=b(l[48][3],R,c),T=[0,g,h,S,d,i,a(e[C],v)],U=[0,g,d,i],V=0,aa=af(aG,T,function(a){return af(bJ,[0,h,q,K,D,j,a],bT)}),W=[0,a(m[67][1],aa),V],X=[0,N(s),W],Y=[0,v],$=ag(u,U,function(a){return aM(Y,a)}),Z=b(p[70][19],$,X),_=a(m[61][1],I);return b(p[70][3],_,Z)}];return a(m[63][9],J)}var
i=n[1],q=b(aF[13],d,[0,-608347012,[0,i[1],i[2]]]),t=b(f[20],d,i[1]),r=b(f[20],d,i[2]),g=i[3];if(typeof
g==="number")return N(q);else
switch(g[0]){case
0:var
au=g[1],E=[0,function(c){var
d=a(f[6],t),h=a(f[6],r),i=a(k[1][6],eC),j=a(l[20],i),g=b(l[48][3],j,c),m=[0,au,[0,a(e[C],g)]],n=a(e[D],m);function
o(c){var
f=[0,a(F[99],n),0],e=[0,c,d,h],i=[0,N(q),f],j=[0,g],k=ag(u,e,function(a){return aM(j,a)});return b(p[70][19],k,i)}return M(b(l[48][7],c,d),o)}];return a(m[63][9],E);case
1:return bU(g[1],t,r,q);default:var
av=g[2],aw=g[1],I=[0,function(d){var
g=a(f[6],r),h=a(k[1][6],eF),i=a(l[20],h),c=b(l[48][3],i,d),j=[0,av,[0,a(e[C],c)]],m=a(e[D],j),n=[0,a(F[99],m),0],o=[0,bU(aw,t,r,q),n],s=aM([0,c],g);return b(p[70][19],s,o)}];return a(m[63][9],I)}}}var
ax=a(c[3],eU);return b(p[70][4],0,ax)}];return a(m[63][9],d)}function
bX(d){var
b=a(c[3],eV);return j(A[6],0,eW,b)}function
bY(d,c){var
e=a(m[67][1],bX),f=bW(d,c),g=a(p[70][29],F[17]),h=b(p[70][3],g,f);return b(p[70][12],h,e)}function
eX(c,d,k,i){var
f=O(c);function
g(n){var
c=[0,function(c){function
o(a){return b(au[2],0,a)}var
f=j(l[48][1],o,c,d),q=f[2],r=f[1],s=a(l[48][5],c),g=bd(bS[4],0,0,0,eY,s,r,q),t=g[1],h=a(e[D],[0,n,[0,g[2],d,k]]),u=a(l[48][5],c),v=B(au[2],0,u,t,h)[1],w=a(i,h),x=a(aN[11],v),y=a(m[67][1],x);return b(p[70][3],y,w)}];return a(m[63][10],c)}var
h=P===f?c[1]:G===f?a(L[2],c):c;return b(p[70][58],h,g)}var
e0=[0,function(t){var
v=a(m[63][3],t);function
w(d,c){try{var
e=0,f=F[85],g=[0],h=function(a){return ag(bK,g,a)}(f),i=[0,a(p[70][22],h),e],j=[0,a(m[13],0),i],k=eX(u,d,c,F[145]),l=b(p[70][19],k,j);return l}catch(c){c=r(c);if(a(m[67][10],c))return b(m[18],0,c);throw c}}function
x(c){var
d=c[1],e=c[2];return d[1]===eZ[1]?a(m[13],0):b(m[18],[0,e],d)}var
f=a(e[Q],v);if(9===f[0]){var
g=f[2];if(3===g.length-1){var
j=O(u),y=f[1],z=g[2],A=g[3],B=P===j?u[1]:G===j?a(L[2],u):u;if(b(av[11],B,y)){var
k=a(e[Q],z),l=a(e[Q],A);if(9===k[0])if(9===l[0]){var
o=l[2],h=k[2];if(h.length-1===o.length-1)var
s=function(c){if(0<=c){var
d=s(c-1|0),e=q(o,c)[c+1],f=w(q(h,c)[c+1],e);return b(p[70][16],f,d)}var
g=bY(bf,0);return a(p[70][22],g)},n=s(h.length-1-1|0),d=1;else
var
d=0}else
var
d=0;else
var
d=0;if(!d)var
n=a(m[13],0);var
i=n,c=1}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
i=a(m[13],0);return b(m[20],i,x)}],ah=[0,N,bW,bX,bY,a(m[63][9],e0)];az(188,ah,"Cc_plugin.Cctac");a(bc[12],e1);var
e2=0,e4=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(an[6],ab[20]),h=b(ac[12][2][7],g,f),i=a(an[17],ab[13]),j=a(an[6],i),k=b(ac[12][2][7],j,e);return function(a){return b(ah[4],h,k)}}}return a(R[2],e3)},e2],e6=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(an[17],ab[13]),f=a(an[6],e),g=b(ac[12][2][7],f,d);return function(a){return b(ah[4],bf,g)}}return a(R[2],e5)},e4],e8=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(an[6],ab[20]),f=b(ac[12][2][7],e,d);return function(a){return b(ah[4],f,0)}}return a(R[2],e7)},e6],e_=[0,function(c){return c?a(R[2],e9):function(a){return b(ah[4],bf,0)}},e8],fa=a(e$[12],e_);j(ac[6][9],0,[0,ai,fb],fa);function
fc(s){var
h=a(k[1][7],fd),b=ab[13],f=0,g=0;if(0===b[0]){var
i=[0,ff,[0,[1,ax[4],[0,[5,[0,b[1]]]],h],g]],l=a(k[1][7],fg),c=ab[20];if(0===c[0]){var
m=[0,[0,fi,[0,[1,ax[4],[5,[0,c[1]]],l],i]],f],o=a(k[1][7],fj),d=ab[13],n=0;if(0===d[0]){var
p=[0,[0,fm,[0,fl,[0,[1,ax[4],[0,[5,[0,d[1]]]],o],n]]],m],r=a(k[1][7],fn),e=ab[20],q=0;if(0===e[0])return j(ac[9][4],[0,ai,fr],0,[0,fq,[0,[0,fp,[0,[1,ax[4],[5,[0,e[1]]],r],q]],p]]);throw[0,as,fo]}throw[0,as,fk]}throw[0,as,fh]}throw[0,as,fe]}b(bc[19],fc,ai);function
fs(d){var
b=[28,[0,0,[31,ax[4],[0,[0,ai,ft],0],0]]],c=a(k[1][6],fu);return B(ac[6][4],1,0,c,b)}var
fv=[0,function(b,a){return ah[5]}];j(ac[6][9],0,[0,ai,fw],fv);b(bc[19],fs,ai);var
bZ=[0,ai];az(195,bZ,"Cc_plugin.G_congruence");az(196,[0,f,aF,ah,bZ],"Cc_plugin");return});
