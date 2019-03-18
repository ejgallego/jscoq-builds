function(e8){"use strict";var
al=104,aC=".",aF=108,bN="  [",bK=3901498,bL=" : ",bV=-431191102,aY=-912009552,U=250,$="Init",N=246,aD="congruence",bR="[",bU="l",bJ="A",bT="with",aE="]",bI=915186972,bQ=888453194,_="Logic",bP=" and ",bO=109,bH=-318868643,bS="Heq",aW="f_equal",aB=15500,bG=129,aX=1e3,bM="n",x=e8.jsoo_runtime,r=x.caml_check_bound,aj=x.caml_int_compare,ay=x.caml_make_vect,d=x.caml_new_string,T=x.caml_obj_tag,ak=x.caml_register_global,s=x.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):x.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):x.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):x.caml_call_gen(a,[b,c,d])}function
B(a,b,c,d,e){return a.length==4?a(b,c,d,e):x.caml_call_gen(a,[b,c,d,e])}function
az(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):x.caml_call_gen(a,[b,c,d,e,f])}function
aA(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):x.caml_call_gen(a,[b,c,d,e,f,g,h])}function
e7(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):x.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}var
h=x.caml_get_global_data(),av=d("cc_plugin"),o=h.Constr,n=h.Names,aa=h.Hashset,a1=h.Sorts,j=h.Util,t=h.Not_found,a8=h.Term,e=h.EConstr,P=h.Typing,g=h.Int,v=h.Stdlib__queue,c=h.Pp,aO=h.Control,D=h.CErrors,bd=h.Environ,bb=h.Assert_failure,p=h.Tacmach,aH=h.Feedback,aZ=h.Printer,ad=h.Stdlib__hashtbl,af=h.Stdlib,M=h.Termops,S=h.CamlinternalLazy,q=h.Tacticals,k=h.Proofview,E=h.Tactics,by=h.Evarsolve,bB=h.DAst,aT=h.Coqlib,bw=h.Equality,bv=h.Context,br=h.CClosure,bs=h.Reductionops,aw=h.Stdarg,ax=h.Genarg,bE=h.Ltac_plugin,aG=[0,0],dr=d("Out of depth ... "),dq=d("Out of instances ... "),ds=d("First run was incomplete, completing ... "),dp=d("Executing ... "),dn=d("Running E-matching algorithm ... "),dm=d("paf_of_patt: pattern is trivial"),dj=d("wrong incomplete class."),dc=d(" ... "),dd=d(" = "),de=d("Checking if "),db=d("Yes"),df=d("No"),c_=d(aC),c$=d("Processing mark for term "),c7=d("weird error in injection subterms merge."),c8=[0,d("add_pacs")],c5=d(aC),c6=d("Updating term "),c2=d(aC),c3=d(bP),c4=d("Merging "),cY=d(aC),cZ=d(bP),c0=d("Linking "),cX=[0,d("plugins/cc/ccalgo.ml"),641,2],cQ=d(aE),cR=d(" <> "),cS=d(bL),cT=d(bN),cU=d("Adding new disequality, depth="),cL=d(aE),cM=d(" == "),cN=d(bL),cO=d(bN),cP=d("Adding new equality, depth="),cK=d("discarding redundant (dis)equality"),cG=d(aE),cH=d(bR),cD=d(aE),cE=d(":="),cF=d(bR),cC=d("incomplete matching."),cs=d("not a node."),ct=[0,d("subterms")],cj=d("not a constructor."),ck=[0,d("get_constructor")],cf=d("not a representative."),cg=[0,d("get_representative")],b7=d("signature already entered."),b8=[0,d("enter")],bY=[0,d("Congruence"),[0,d("Verbose"),0]],bZ=d("Congruence Verbose"),cp=d("Ccalgo.Discriminable"),cv=d(bJ),cx=d(bJ),dg=d("_eps_"),dt=d("invalid cc transitivity."),du=d("not enough args."),dv=[0,d("nth_arg")],dw=[0,1,20],dx=d("equal_proof "),dy=[0,1,20],dz=d("edge_proof "),dA=[0,1,20],dB=d("constr_proof "),dD=d(","),dC=d("}"),dE=d("{"),dF=[0,1,20],dG=d("path_proof "),dH=[0,1,20],dI=d("congr_proof "),dJ=[0,1,20],dK=d("ind_proof "),d2=[0,0],d9=[0,d("plugins/cc/cctac.ml"),254,9],ed=d("f"),ee=d("I don't know how to handle dependent equality"),eE=[0,0],ev=d("("),ew=d(")"),eq=[0,1],eo=d("Goal solved, generating proof ..."),en=d("Computation completed."),em=d("Problem built, solving ..."),el=d("Reading subgoal ..."),ep=[13,0,0,0],es=d("Goal is solvable by congruence but some arguments are missing."),et=d("  replacing metavariables by arbitrary terms."),eu=d(')",'),ex=d('"congruence with ('),ey=d("  Try "),ez=d("Incomplete"),eA=d("congruence failed"),ek=d(bS),ej=d("H"),eh=d("e"),ei=d("X"),ef=d(bS),ea=[0,0],eb=[0,1],d8=d("t"),dL=d("CC"),dM=d(aW),dN=[0,d($),[0,d(_),0]],dO=d("eq_rect"),dP=[0,d($),[0,d(_),0]],dR=d("eq_refl"),dS=[0,d($),[0,d(_),0]],dT=d("eq_sym"),dU=[0,d($),[0,d(_),0]],dW=d("eq_trans"),dX=[0,d($),[0,d(_),0]],dY=d("eq"),dZ=[0,d($),[0,d(_),0]],d0=d("False"),d1=[0,d($),[0,d(_),0]],eB=d("congruence failed."),eL=d(bU),eM=d(bT),eO=d(bM),eP=d(aD),eS=d(bU),eT=d(bT),eU=d(aD),eX=d(bM),eY=d(aD),e0=[0,d(aD),0],e2=d("cc"),e4=[0,d(aW),0],e6=d(aW),dk=h.Vars,di=h.Namegen,bW=h.Pfedit,b1=h.Goptions,d3=h.Global,d4=h.Inductiveops,d5=h.Retyping,d_=h.Evarutil,eG=h.Pretype_errors,eH=h.Type_errors,er=h.Detyping,ec=h.Evd,d$=h.Refine,eI=h.Mltop,V=5;function
am(c){var
a=b(bW[6],0,0);return i(aZ[15],a[2],a[1],c)}function
z(d){var
c=aG[1];if(c){var
e=a(d,0);return b(aH[10],0,e)}return c}function
bX(a){aG[1]=a;return 0}var
b0=[0,0,bZ,bY,function(a){return aG[1]},bX];b(b1[4],0,b0);var
b2=g[1],b3=[0,function(b,a){return b===a?1:0},b2],an=a(ad[25],b3);function
b4(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
b5=[0,b4,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b(aa[2][1],f,e)}],ae=a(ad[25],b5);function
b6(f,e,d){if(b(ae[11],d[1],e)){var
g=a(c[3],b7);i(D[3],0,b8,g)}else
i(ae[10],d[1],e,f);return i(an[10],d[2],f,e)}function
b9(c,a){return b(ae[7],a[1],c)}function
b_(a,c){try{var
d=b(an[7],a[2],c);b(ae[6],a[1],d);var
e=b(an[6],a[2],c);return e}catch(a){a=s(a);if(a===t)return 0;throw a}}function
b$(c,a){function
d(a){return b_(c,a)}return b(g[2][13],d,a)}var
ca=[0,function(b,a){var
c=aj(b[1],a[1]),e=a[3],f=a[2],g=b[3],h=b[2];if(0===c){var
d=aj(h,f);return 0===d?i(j[17][46],aj,g,e):d}return c}],cb=[0,function(b,a){var
c=aj(b[1],a[1]),d=a[2],e=b[2];return 0===c?aj(e,d):c}],m=a(j[21][1],ca),l=a(j[21][1],cb);function
a0(b,a){var
c=typeof
b==="number"?0===b?typeof
a==="number"?0===a?1:0:0:typeof
a==="number"?0===a?0:1:0:typeof
a==="number"?0:1;return c?1:0}function
aI(l,k){var
c=l,a=k;for(;;){switch(c[0]){case
0:if(0===a[0])return b(o[79],c[1],a[1]);break;case
1:if(1===a[0]){var
m=a[2],p=c[2],f=a0(c[1],a[1]);return f?a0(p,m):f}break;case
2:if(2===a[0])return b(n[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],g=aI(c[1],a[1]);if(g){var
c=r,a=q;continue}return g}break;default:if(4===a[0]){var
d=a[1],e=c[1],h=e[2]===d[2]?1:0,s=d[3],t=d[1][1],u=e[3],v=e[1][1];if(h){var
i=u===s?1:0;if(i)return b(n[46],v,t);var
j=i}else
var
j=h;return j}}return 0}}function
aJ(c){switch(c[0]){case
0:var
e=a(o[93],c[1]);return b(aa[2][1],1,e);case
1:var
f=c[1],g=a(a1[6],c[2]),h=a(a1[6],f);return i(aa[2][3],2,h,g);case
2:var
j=a(n[1][3],c[1]);return b(aa[2][1],3,j);case
3:var
k=c[1],l=aJ(c[2]),m=aJ(k);return i(aa[2][3],4,m,l);default:var
d=c[1],p=d[3],q=d[2],r=a(n[50],d[1][1]);return B(aa[2][4],5,r,q,p)}}var
F=a(ad[25],[0,o[79],o[93]]),W=a(ad[25],[0,aI,aJ]),aK=a(ad[25],[0,n[1][1],n[1][3]]),cc=[0,a(o[1],af[9])],a2=[0,[1,af[9],[0,af[9],af[9],0]],af[9],m[1],0,cc];function
a3(c){var
b=a(W[1],V);return[0,V,0,ay(5,a2),a(F[1],V),0,b]}function
cd(e,b){var
f=a(p[2],b),h=a(p[8],b),i=a(F[1],V),j=a(aK[1],V),k=g[2][1],l=a(v[2],0),m=a(v[2],0),n=g[2][1],c=a(an[1],V),d=[0,a(ae[1],V),c];return[0,a3(0),d,n,m,l,0,0,k,j,e,0,i,h,f]}function
ce(a){return a[1]}function
y(e,g){var
c=0,a=g;for(;;){var
d=r(e[3],a)[a+1][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){r(e[3],b)[b+1][2]=a;return 0};b(j[17][11],f,c);return a}}function
J(e,b){var
d=r(e[3],b)[b+1][1];if(0===d[0])return d[1];var
f=a(c[3],cf);return i(D[3],0,cg,f)}function
aL(b,a){return r(b[3],a)[a+1][3]}function
ch(d,c,a){var
e=aL(d,c);return b(m[22],a,e)}function
ci(c,f,e){var
a=f;for(;;)try{var
g=aL(c,a),h=b(m[22],e,g);return h}catch(b){b=s(b);if(b===t){var
d=r(c[3],a)[a+1][1];if(0===d[0])throw t;var
a=d[1];continue}throw b}}function
a4(e,b){var
d=r(e[3],b)[b+1][5];if(4===d[0])return d[1];var
f=a(c[3],cj);return i(D[3],0,ck,f)}function
a5(b,a){return J(b,a)[1]}function
cl(a){return a[4]}function
cm(a){return a[5]}function
cn(e,d,c){var
a=J(e,d);a[1]=a[1]+1|0;a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
co(e,d,c){var
a=J(e,d);a[1]=a[1]+1|0;a[3]=b(g[2][4],c,a[3]);return 0}var
a6=[248,cp,x.caml_fresh_oo_id(0)];function
cq(b){var
c=a(j[17][6],b[3]);return[0,b[1],b[2]+1|0,c]}function
cr(a,c,e){try{var
j=b(l[22],c,a[6]),d=j}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=i(l[4],c,h,f);return 0}function
ab(b,a){return r(b[3],a)[a+1][5]}function
aM(f,b){var
d=r(f[3],b)[b+1][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cs);return i(D[3],0,ct,g)}function
a7(a,c){var
b=aM(a,c),d=b[1],e=y(a,b[2]);return[0,y(a,d),e]}function
cu(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=ay(d,a2);a[1]=d;az(j[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
ao(a){return[0,0,g[2][1],g[2][1],0,a,l[1]]}var
cw=[0,a(n[1][6],cv)],cy=[0,a(n[1][6],cx)],cz=a(o[1],2),cA=[0,0,a(o[1],2),cz],cB=a(o[10],cA);function
G(b){switch(b[0]){case
0:return b[1];case
1:var
h=b[1],e=[0,cy,a(o[5],b[2]),cB],f=a(o[11],e),g=[0,cw,a(o[5],h),f];return a(o[11],g);case
2:return a(o[2],b[1]);case
3:var
i=b[1],d=[0,G(b[2]),0],c=i;for(;;){if(3===c[0]){var
k=c[1],d=[0,G(c[2]),d],c=k;continue}var
j=[0,G(c),d];return a(a8[11],j)}default:return a(o[21],b[1][1])}}function
a9(q,p){var
f=a(e[bG][1],p);function
d(b){return a9(q,a(e[8],b))}var
c=a(o[26],f);switch(c[0]){case
6:var
r=c[2],s=c[1],t=d(c[3]),u=[0,s,d(r),t];return a(o[10],u);case
7:var
v=c[2],w=c[1],x=d(c[3]),y=[0,w,d(v),x];return a(o[11],y);case
8:var
z=c[3],A=c[2],B=c[1],C=d(c[4]),D=d(z),E=[0,B,d(A),D,C];return a(o[12],E);case
9:var
F=c[1],G=b(j[19][75][1],d,c[2]),H=[0,d(F),G];return a(o[13],H);case
10:var
g=c[1],I=g[2],J=a(n[17][6],g[1]),K=[0,a(n[17][2],J),I];return a(o[16],K);case
11:var
h=c[1],i=h[1],L=h[2],M=i[2],N=a(n[23][6],i[1]),O=[0,[0,a(n[23][2],N),M],L];return a(o[19],O);case
12:var
k=c[1],l=k[1],m=l[1],P=k[2],Q=l[2],R=m[2],S=a(n[23][6],m[1]),T=[0,[0,[0,a(n[23][2],S),R],Q],P];return a(o[21],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(n[23][6],b);return a(n[23][2],c)},X=b(n[67][18],W,V),Y=[0,X,d(U)];return a(o[17],Y);default:return f}}function
aN(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,aN(b,c)]};return i(j[17][16],f,d,e)}var
c=a[1]-1|0;return r(b,c)[c+1]}function
K(f,d){var
g=a(c[3],cD),h=G(ab(f,d)),i=am(a(e[8],h)),j=a(c[3],cE),k=a(c[16],d),l=a(c[3],cF),m=b(c[12],l,k),n=b(c[12],m,j),o=b(c[12],n,i);return b(c[12],o,g)}function
ap(d){var
f=a(c[3],cG),g=G(d),h=am(a(e[8],g)),i=a(c[3],cH),j=b(c[12],i,h);return b(c[12],j,f)}function
O(d,f){var
h=d[1];try{var
k=b(W[7],h[6],f);return k}catch(k){k=s(k);if(k===t){var
c=cu(h),q=G(f),u=a(e[8],q),w=i(P[1],d[13],d[14],u),j=a9(d[14],w);switch(f[0]){case
2:var
B=m[1],l=[0,[0,ao(j)],-1,B,0,f];break;case
3:var
C=f[2],o=O(d,f[1]),p=O(d,C);cn(h,y(h,o),c);co(h,y(h,p),c);d[3]=b(g[2][4],c,d[3]);var
D=m[1],l=[0,[0,ao(j)],-1,D,[0,[0,o,p]],f];break;case
4:var
E=f[1];b(v[3],[0,c,[0,[0,c,0]]],d[5]);b(v[3],[0,c,[1,[0,c,E[2],0]]],d[5]);var
H=m[1],l=[0,[0,ao(j)],-1,H,0,f];break;default:b(v[3],[0,c,[0,[0,c,0]]],d[5]);var
x=m[1],l=[0,[0,ao(j)],-1,x,0,f]}r(h[3],c)[c+1]=l;i(W[5],h[6],f,c);try{var
A=b(F[7],d[12],j),n=A}catch(a){a=s(a);if(a!==t)throw a;var
n=g[2][1]}var
z=b(g[2][4],c,n);i(F[10],d[12],j,z);return c}throw k}}function
a_(a,e,d,c){var
f=O(a,d),g=O(a,c);b(v[3],[0,f,g,[0,e,0]],a[4]);return i(F[5],a[1][4],e,[0,d,c])}function
a$(a,d,c,b){var
e=O(a,c),f=O(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
cI(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
cJ(a,d,c){try{var
e=a[1],f=function(a){return y(e,a)},g=b(j[19][15],f,c),h=b(aK[9],a[9],d),k=function(b){function
c(c,b){return c===y(a[1],b)?1:0}return i(j[19][35],c,g,b)},l=b(j[17][22],k,h);return l}catch(a){a=s(a);if(a===t)return 0;throw a}}function
cV(e,b,a,d){var
c=r(e[3],b)[b+1];c[1]=[1,a,d];c[2]=a;return 0}function
ba(g,f,e){var
a=f,b=e;for(;;){var
c=r(g[3],a)[a+1][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
cW(c,i,h){var
o=y(c,h);if(y(c,i)===o){var
p=ba(c,h,0),a=[0,ba(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,bb,cX]}function
bc(d,h,k,w){z(function(o){var
e=a(c[3],cY),f=K(d[1],k),g=a(c[3],cZ),i=K(d[1],h),j=a(c[3],c0),l=b(c[12],j,i),m=b(c[12],l,g),n=b(c[12],m,f);return b(c[12],n,e)});var
j=J(d[1],h),e=J(d[1],k);cV(d[1],h,k,w);try{var
H=b(F[7],d[12],j[5]),p=H}catch(a){a=s(a);if(a!==t)throw a;var
p=g[2][1]}var
x=b(g[2][6],h,p);i(F[10],d[12],j[5],x);var
q=b(g[2][7],j[3],e[3]);e[1]=a(g[2][20],q);e[3]=q;e[2]=b(g[2][7],j[2],e[2]);b$(d[2],j[3]);d[3]=b(g[2][7],d[3],j[3]);var
y=r(d[1][3],h)[h+1][3];function
A(c,a){return b(v[3],[0,a,[1,c]],d[5])}b(m[10],A,y);var
B=j[6];function
C(c){function
e(a){return b(v[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(l[10],C,B);var
n=j[4],f=e[4];if(typeof
n==="number"){if(0===n)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],k,d[8]);e[4]=1;return 0}}else
if(0===n[0]){var
D=n[1];if(typeof
f==="number"){if(0===f){e[4]=[0,D];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],k,d[8]);return 0}var
u=0}else
var
u=1===f[0]?1:0;if(!u){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
o=n[1],E=o[2],G=o[1];if(typeof
f==="number"){if(0===f){e[4]=[1,o];return 0}}else
if(0!==f[0])return b(v[3],[0,G,[1,E]],d[5])}return 0}function
c1(e,f){z(function(n){var
d=a(c[3],c2),g=K(f[1],e[2]),h=a(c[3],c3),i=K(f[1],e[1]),j=a(c[3],c4),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=f[1],h=y(g,e[1]),i=y(g,e[2]),j=1-(h===i?1:0);if(j){var
l=a5(g,i);if(a5(g,h)<l)return bc(f,h,i,e);var
d=e[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bc(f,i,h,[0,e[2],e[1],k])}return j}function
c9(f,s,d){z(function(j){var
e=a(c[3],c_),g=K(d[1],f),h=a(c[3],c$),i=b(c[12],h,g);return b(c[12],i,e)});var
p=y(d[1],f),h=J(d[1],p);if(0===s[0]){cr(h,s[1],f);d[3]=b(g[2][7],h[2],d[3]);return 0}var
e=s[1],q=r(d[1][3],p)[p+1];if(1-b(m[3],e,q[3]))q[3]=i(m[4],e,f,q[3]);var
j=h[4];if(typeof
j==="number"){if(0===j)return 0===e[2]?(h[4]=[1,[0,f,e]],0):(d[3]=b(g[2][7],h[2],d[3]),h[4]=[0,e],d[8]=b(g[2][4],p,d[8]),0)}else
if(1===j[0]){var
t=j[1],k=t[2],u=t[1];if(e[1]===k[1]){var
x=a4(d[1],e[1]),o=x[3],n=k[3],l=e[3];for(;;){var
w=0<o?1:0;if(w){if(n)if(l){var
A=l[2],B=n[2];b(v[3],[0,n[1],l[1],[1,u,k,f,e,o]],d[4]);var
o=o-1|0,n=B,l=A;continue}var
C=a(c[3],c7);return i(D[3],0,c8,C)}return w}}throw[0,a6,u,k,f,e]}d[3]=b(g[2][7],h[2],d[3]);return 0}function
da(e){var
g=e[1];function
h(f){if(f){var
d=f[1],k=f[2],l=y(g,d[2]);if(y(g,d[1])===l)var
j=a(c[3],db),i=[0,d];else
var
m=h(k),j=a(c[3],df),i=m;z(function(p){var
f=a(c[3],dc),g=K(e[1],d[2]),h=a(c[3],dd),i=K(e[1],d[1]),k=a(c[3],de),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(e[6])}var
dh=a(n[1][6],dg);function
dl(d){var
f=d[8];function
h(p){var
h=J(d[1],p)[4];if(typeof
h!=="number"&&0===h[0]){var
f=h[1],w=G(ab(d[1],f[1])),x=a(e[8],w),y=i(P[1],d[13],d[14],x),z=a(e[bG][1],y),A=f[3],B=function(a){return G(ab(d[1],a))},C=b(j[17][69],B,A),E=a(j[17][9],C),F=b(a8[28],z,E),H=f[2],l=ab(d[1],p),m=F,k=H;for(;;){if(0<k){var
n=a(o[60],m),t=n[3],u=a(e[8],n[2]),q=a(bd[11],d[13]),r=a(bd[32],q),g=b(di[26],dh,r);d[13]=b(e[112],[0,g,u],d[13]);var
v=[0,a(o[2],g),0],l=[3,l,[2,g]],m=b(dk[13],v,t),k=k-1|0;continue}d[1][5]=[0,f,d[1][5]];O(d,l);return 0}}var
s=a(c[3],dj);return i(D[3],0,0,s)}return b(g[2][13],h,f)}function
be(c){var
a=[0,l[1]],f=c[1],d=c[1][3];function
e(c,h){var
d=c<f[2]?1:0;if(d){var
e=h[1];if(0===e[0]){var
j=e[1][6],k=function(d,k){try{var
j=b(l[22],d,a[1]),e=j}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}var
f=a[1],h=b(g[2][4],c,e);a[1]=i(l[4],d,h,f);return 0};return b(l[10],k,j)}return 0}return d}b(j[19][14],e,d);return a[1]}function
bf(q,p,d){var
c=a(j[22][9],d),m=c[3];if(m){var
e=m[2],u=m[1],f=u[2],h=u[1],i=q[1];if(0===h[0]){var
k=h[2],n=h[1];if(k){var
A=k[2],B=k[1];try{var
C=b(W[7],i[6],n),D=[0,C,a(j[17][1],k)],E=J(i,f)[6],F=b(l[22],D,E),G=function(g){var
f=a7(q[1],g),h=[0,[0,[0,n,A],f[1]],[0,[0,B,f[2]],e]],i=c[2],k=[0,a(j[19][8],c[1]),i,h];return b(j[22][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=s(a);if(a===t)return 0;throw a}}try{var
v=y(i,b(W[7],i[6],n))===f?1:0,I=v?b(j[22][3],[0,c[1],c[2],e],d):v;return I}catch(a){a=s(a);if(a===t)return 0;throw a}}var
o=h[1],w=o-1|0;if(0<=r(c[1],w)[w+1]){var
x=o-1|0;return r(c[1],x)[x+1]===f?b(j[22][3],[0,c[1],c[2],e],d):0}var
z=o-1|0;r(c[1],z)[z+1]=f;return b(j[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
aP(d,c){if(0===c[0]){var
e=c[1],f=a(j[17][1],c[2]);return[0,b(W[7],d,e),f]}return b(D[9],0,dm)}function
bg(c){var
k=c[1][6],f=a(j[22][2],0),m=be(c);function
d(a){var
h=a[5];if(typeof
h==="number")if(0===h)try{var
x=aP(k,a[4]),y=b(l[22],x,m),d=y}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=h[1];try{var
A=b(F[7],c[12],z),o=A}catch(a){a=s(a);if(a!==t)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(j[22][3],[0,ay(a[3],-1),a,[0,[0,a[4],c],0]],f)}b(g[2][13],p,d);var
i=a[7];if(typeof
i==="number")if(0===i)try{var
r=aP(k,a[6]),u=b(l[22],r,m),e=u}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}else
var
e=g[2][1];else{var
v=i[1];try{var
w=b(F[7],c[12],v),n=w}catch(a){a=s(a);if(a!==t)throw a;var
n=g[2][1]}var
e=n}function
q(c){return b(j[22][3],[0,ay(a[3],-1),a,[0,[0,a[6],c],0]],f)}return b(g[2][13],q,e)}b(j[17][11],d,c[7]);return f}function
bh(b){var
d=[0,0],e=bg(b);z(function(b){return a(c[3],dn)});try{for(;;){a(aO[3],0);bf(b,d,e);continue}}catch(a){a=s(a);if(a===j[22][1])return d[1];throw a}}function
aQ(w,d){z(function(b){return a(c[3],dp)});try{for(;;){a(aO[3],0);try{c1(a(v[5],d[4]),d);var
L=1,h=L}catch(e){e=s(e);if(e!==v[1])throw e;try{var
u=a(v[5],d[5]);c9(u[1],u[2],d);var
I=1,h=I}catch(e){e=s(e);if(e!==v[1])throw e;try{var
f=a(g[2][26],d[3]);d[3]=b(g[2][6],f,d[3]);z(function(i){return function(j){var
e=a(c[3],c5),f=K(d[1],i),g=a(c[3],c6),h=b(c[12],g,f);return b(c[12],h,e)}}(f));var
n=a7(d[1],f),p=n[1],y=aM(d[1],f)[2],q=J(d[1],p),r=q[4],W=typeof
r==="number"?0:0===r[0]?(q[4]=1,d[8]=b(g[2][6],p,d[8]),1):0,A=aL(d[1],p),B=function(c,e){return function(a,f){return b(v[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(y,f);b(m[10],B,A);var
C=q[6],E=function(c){return function(a,e){return b(v[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(f);b(l[10],E,C);try{var
F=b9(n,d[2]);b(v[3],[0,f,F,0],d[4])}catch(a){a=s(a);if(a!==t)throw a;b6(f,n,d[2])}var
H=1,h=H}catch(a){a=s(a);if(a!==t)throw a;var
h=0}}}if(h)continue;var
x=da(d);if(x)var
R=x[1],S=w?[1,R]:0,k=[0,S];else
if(a(g[2][2],d[8]))if(0<d[10]){var
T=bh(d),U=function(q){var
m=q[2],f=q[1];a(aO[3],0);var
n=0<d[10]?1:0;if(n){if(cJ(d,f[1],m))return z(function(b){return a(c[3],cK)});i(aK[5],d[9],f[1],m);var
t=d[1],r=function(b){try{var
e=ab(t,b);return e}catch(b){b=s(b);if(a(D[18],b)){var
d=a(c[3],cC);return i(D[3],0,0,d)}throw b}},l=b(j[19][15],r,m),u=a(o[2],f[1]),p=b(j[19][15],G,l);a(j[19][44],p);var
g=a(o[13],[0,u,p]),h=aN(l,f[4]),k=aN(l,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(z(function(A){var
f=a(c[3],cL),i=ap(k),j=a(c[3],cM),l=ap(h),m=a(c[3],cN),n=am(a(e[8],g)),o=a(c[3],cO),p=b(c[12],o,n),q=b(c[12],p,m),r=b(c[12],q,l),s=b(c[12],r,j),t=b(c[12],s,i),u=b(c[12],t,f),v=a(c[5],0),w=a(c[16],d[10]),x=a(c[3],cP),y=b(c[12],x,w),z=b(c[12],y,v);return b(c[12],z,u)}),a_(d,g,h,k)):(z(function(A){var
f=a(c[3],cQ),i=ap(k),j=a(c[3],cR),l=ap(h),m=a(c[3],cS),n=am(a(e[8],g)),o=a(c[3],cT),p=b(c[12],o,n),q=b(c[12],p,m),r=b(c[12],q,l),s=b(c[12],r,j),t=b(c[12],s,i),u=b(c[12],t,f),v=a(c[5],0),w=a(c[16],d[10]),x=a(c[3],cU),y=b(c[12],x,w),z=b(c[12],y,v);return b(c[12],z,u)}),a$(d,[0,g],h,k))}return n};b(j[17][11],U,T);var
V=d[11]?(d[11]=0,aQ(1,d)):(z(function(b){return a(c[3],dq)}),0),k=V}else{z(function(b){return a(c[3],dr)});var
k=0}else{z(function(b){return a(c[3],ds)});dl(d);var
k=aQ(0,d)}return k}}catch(a){a=s(a);if(a[1]===a6){var
M=a[5],N=a[4],O=a[3],P=a[2],Q=w?[0,[0,P,O,N,M]]:0;return[0,Q]}throw a}}var
f=[0,[0,l[1],l[2],l[3],l[4],l[5],l[6],l[7],l[8],l[9],l[10],l[11],l[12],l[13],l[14],l[15],l[16],l[17],l[18],l[19],l[20],l[21],l[22],l[23],l[24]],[0,m[1],m[2],m[3],m[4],m[5],m[6],m[7],m[8],m[9],m[10],m[11],m[12],m[13],m[14],m[15],m[16],m[17],m[18],m[19],m[20],m[21],m[22],m[23],m[24]],F,W,aI,G,z,ce,cl,cm,cd,O,a_,a$,cI,cq,y,ch,ci,ab,a4,aM,cW,be,bf,bg,aP,bh,aQ,K,a3];ak(162,f,"Cc_plugin.Ccalgo");function
ag(a){return[0,a,a,[2,a]]}function
ac(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return ag([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
A(o,n){var
e=o,d=n;for(;;){var
g=e[3],h=d[3];switch(g[0]){case
2:return d;case
4:var
l=g[2],m=g[1];switch(h[0]){case
2:var
j=0;break;case
3:var
k=h[1][3];if(4===k[0]){var
r=h[2],s=k[1],t=A(l,k[2]),e=ac(A(m,s),t),d=r;continue}var
j=1;break;case
4:var
u=h[1],v=A(l,h[2]);return ac(A(m,u),v);default:var
j=1}break;default:var
j=0}if(!j){if(2===h[0])return e;if(3===g[0]){var
q=g[1],e=q,d=A(g[2],d);continue}}if(b(f[5],e[2],d[1]))return[0,e[1],d[2],[3,e,d]];var
p=a(c[3],dt);return i(D[3],0,0,p)}}function
L(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=L(a[1]);return A(L(c),d);case
4:var
e=a[1],f=L(a[2]);return ac(L(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,L(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bi(d,a){var
c=b(f[3][7],d,a);return[0,c[1],c[2],[0,a]]}function
bj(d,a){var
c=b(f[3][7],d,a);return[0,c[2],c[1],[1,a]]}function
bk(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],j=b[1];if(0<d){var
b=j,d=d-1|0;continue}return h}var
g=a(c[3],du);return i(D[3],0,dv,g)}}function
bl(c,d,b,a){var
e=bk(c[2],b-a|0);return[0,bk(c[1],b-a|0),e,[5,c,d,b,a]]}function
Q(d,e,g){function
j(n){var
h=b(f[30],d,g),i=a(c[4],dw),j=b(f[30],d,e),k=a(c[3],dx),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);if(e===g)return ag(b(f[20],d,e));var
h=i(f[23],d,e,g),k=h[1],l=L(aq(d,g,h[2]));return A(aq(d,e,k),l)}function
bm(d,i){var
g=i[2],j=i[1],k=j[2],l=j[1];function
p(n){var
e=b(f[30],d,k),g=a(c[4],dy),h=b(f[30],d,l),i=a(c[3],dz),j=b(c[12],i,h),m=b(c[12],j,g);return b(c[12],m,e)}a(f[7],p);var
q=Q(d,l,g[1]),r=L(Q(d,k,g[2])),e=g[3];if(typeof
e==="number")var
h=bn(d,g[1],g[2]);else
if(0===e[0])var
m=e[1],s=e[2]?bj(a(f[9],d),m):bi(a(f[9],d),m),h=s;else
var
n=e[2],t=e[5],u=aS(d,e[1],n,e[3],e[4]),o=b(f[21],d,n[1]),h=bl(u,o[1],o[3],t);return A(A(q,h),r)}function
aR(d,g,e){function
l(k){var
e=a(c[4],dA),h=b(f[30],d,g),i=a(c[3],dB),j=b(c[12],i,h);return b(c[12],j,e)}a(f[7],l);var
h=i(f[19],d,g,e),j=Q(d,g,h);if(0===e[3])return j;var
m=a(f[16],e),k=b(f[22],d,h),n=k[1],o=b(f[20],d,k[2]),p=aR(d,n,m);return A(j,ac(p,ag(o)))}function
aq(e,g,d){function
j(u){var
h=a(c[3],dC);function
j(b){return a(c[16],b[1][2])}function
k(b){return a(c[3],dD)}var
l=i(c[39],k,j,d),m=a(c[3],dE),n=a(c[4],dF),o=b(f[30],e,g),p=a(c[3],dG),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m),t=b(c[12],s,l);return b(c[12],t,h)}a(f[7],j);if(d){var
h=d[1],k=d[2],l=bm(e,h);return A(aq(e,h[1][2],k),l)}return ag(b(f[20],e,g))}function
bn(d,g,e){function
j(n){var
h=b(f[30],d,e),i=a(c[4],dH),j=b(f[30],d,g),k=a(c[3],dI),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
h=b(f[22],d,g),k=h[2],l=h[1],i=b(f[22],d,e),m=i[1],n=Q(d,k,i[2]);return ac(Q(d,l,m),n)}function
aS(d,g,i,e,h){function
j(n){var
h=b(f[30],d,e),i=a(c[4],dJ),j=b(f[30],d,g),k=a(c[3],dK),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
k=Q(d,g,e),l=aR(d,g,i),m=A(k,aR(d,e,h));return A(L(l),m)}var
ar=[0,ag,ac,A,L,bi,bj,bl,Q,bm,aq,bn,aS,function(c,b){if(bH<=b[1]){var
a=b[2];return aS(c,a[1],a[2],a[3],a[4])}var
d=b[2];return Q(c,d[1],d[2])}];ak(163,ar,"Cc_plugin.Ccproof");function
X(b,a){return[N,function(c){return i(aT[2],dL,b,a)}]}var
aU=X(dN,dM),dQ=X(dP,dO),bo=X(dS,dR),dV=X(dU,dT),bp=X(dX,dW),w=X(dZ,dY),R=X(d1,d0);function
bq(c,b,a){return B(bs[17],br[13],c,b,a)}function
as(c,b,a){return B(bs[17],br[8],c,b,a)}function
at(c,b,a){return i(P[4],c,b,a)[2]}function
C(f,c,z){var
g=z;for(;;){var
A=bq(f,c,g),d=b(e[3],c,A);switch(d[0]){case
6:var
l=d[3],m=d[2];if(i(e[aF][13],c,1,l)){var
p=a(M[47],l),B=at(f,c,p),D=at(f,c,m),E=C(f,c,p);return[3,[3,[1,D,B],C(f,c,m)],E]}break;case
9:var
F=d[2],G=C(f,c,d[1]),H=function(a){return C(f,c,a)},I=b(j[19][15],H,F),J=function(b,a){return[3,b,a]};return i(j[19][17],J,G,I);case
10:var
q=d[1],K=q[1],L=b(e[2][2],c,q[2]),N=a(n[17][6],K),O=[0,a(n[17][2],N),L];return[0,a(o[16],O)];case
11:var
r=d[1],s=r[1],P=s[2],Q=s[1],R=b(e[2][2],c,r[2]),S=a(n[23][6],Q),T=[0,[0,a(n[23][2],S),P],R];return[0,a(o[19],T)];case
12:var
u=d[1],v=u[1],w=v[2],x=v[1],U=x[2],V=x[1],W=b(e[2][2],c,u[2]),X=a(n[23][6],V),h=[0,a(n[23][2],X),U],Y=a(d3[28],h)[1],y=b(d4[44],f,[0,h,w]);return[4,[0,[0,[0,h,w],W],y,y-Y[6]|0]];case
16:var
Z=d[2],_=d[1],$=function(b){var
c=a(n[23][6],b);return a(n[23][2],c)},aa=b(n[67][18],$,_),g=az(d5[9],f,c,aa,Z,0);continue}var
k=b(M[60],c,g);if(b(e[aF][16],c,k))return[0,i(e[5],d2,c,k)];throw t}}function
aV(d,c,g){var
k=as(d,c,g),h=b(e[3],c,k);if(9===h[0]){var
f=h[2],j=T(w),l=h[1],m=U===j?w[1]:N===j?a(S[2],w):w;if(i(M[al],c,m,l))if(3===f.length-1){var
n=C(d,c,r(f,2)[3]),o=C(d,c,r(f,1)[2]);return[0,aB,[0,r(f,0)[1],o,n]]}return[0,aY,C(d,c,g)]}return[0,aY,C(d,c,g)]}function
ah(d,c,h){var
r=bq(d,c,h),f=b(e[3],c,r);switch(f[0]){case
0:var
k=f[1];return[0,[1,k],a(g[2][5],k)];case
6:var
l=f[3],m=f[2];if(i(e[aF][13],c,1,l)){var
n=a(M[47],l),o=ah(d,c,m),t=o[2],u=o[1],p=ah(d,c,n),v=p[2],w=p[1],x=at(d,c,n),y=at(d,c,m);return[0,[0,[1,y,x],[0,u,[0,w,0]]],b(g[2][7],t,v)]}break;case
9:var
z=f[2],A=C(d,c,f[1]),B=function(a){return ah(d,c,a)},D=b(j[19][53],B,z),q=a(j[17][123],D),E=q[1],F=i(j[17][15],g[2][7],g[2][1],q[2]);return[0,[0,A,a(j[17][9],E)],F]}var
s=C(d,c,h);return[0,[0,s,0],g[2][1]]}function
bt(a){return 0===a[0]?1:0}function
bu(k,c,j,v){try{var
x=as(k,c,v),l=b(e[74],c,x)}catch(a){a=s(a);if(a===o[54])throw t;throw a}var
d=l[2],m=T(w),y=l[1],z=U===m?w[1]:N===m?a(S[2],w):w;if(i(M[al],c,z,y))if(3===d.length-1){var
n=ah(k,c,r(d,1)[2]),p=n[1],A=n[2],q=ah(k,c,r(d,2)[3]),u=q[1],B=q[2];if(a(g[2][20],A)===j)if(bt(p))var
f=0;else
var
D=r(d,0)[1],f=[0,i(e[5],0,c,D)];else
var
f=1;if(a(g[2][20],B)===j)if(bt(u))var
h=0;else
var
C=r(d,0)[1],h=[0,i(e[5],0,c,C)];else
var
h=1;if(1===f)if(1===h)throw t;return[0,j,f,p,h,u]}throw t}function
d6(o,c,n,m){var
d=o,f=n,h=m;for(;;){var
p=as(d,c,h),g=b(e[3],c,p);if(6===g[0]){var
j=g[3],k=g[2],l=T(R),q=g[1],r=U===l?R[1]:N===l?a(S[2],R):R;if(i(M[al],c,r,j))return[0,bQ,bu(d,c,f,k)];var
d=b(e[bO],[0,q,k],d),f=f+1|0,h=j;continue}return[0,bI,bu(d,c,f,h)]}}function
d7(d,c,g){var
n=as(d,c,g),f=b(e[3],c,n);if(6===f[0]){var
k=f[3],l=f[2],m=T(R),o=f[1],p=U===m?R[1]:N===m?a(S[2],R):R;if(i(M[al],c,p,k)){var
h=aV(d,c,l);if(aB<=h[1]){var
j=h[2];return[0,bK,[0,j[1],j[2],j[3]]]}return[0,bV,h[2]]}try{var
q=d6(b(e[bO],[0,o,l],d),c,1,k);return q}catch(a){a=s(a);if(a===t)return[0,aY,C(d,c,g)];throw a}}return aV(d,c,g)}function
Y(c,g,f){var
d=T(c);function
h(b){return a(f,a(e[21],[0,b,g]))}var
i=U===d?c[1]:N===d?a(S[2],c):c,j=a(q[66][61],i);return b(k[72][1],j,h)}function
ai(c,n,v){function
d(h){var
d=T(c);function
f(o){var
q=a(k[67][4],h),w=a(k[67][2],h);function
c(r){var
x=b(p[42][8],h,o),y=a(j[19][11],n),z=i(M[58],r,x,y),c=r,k=z,f=v,d=0,A=a(e[21],[0,o,n]);for(;;){if(0===f){var
C=[0,A,a(j[17][9],d)],s=a(e[34],C);return[0,B(P[6],q,c,s,w),s]}var
g=b(e[3],c,k);if(6===g[0]){var
t=g[3],l=e7(d_[4],0,0,0,0,0,0,0,q,c,g[2]),m=l[2],u=l[1],c=u,k=b(e[aF][5],m,t),f=f-1|0,d=[0,m,d];continue}throw[0,bb,d9]}}return b(d$[2],0,c)}var
g=U===d?c[1]:N===d?a(S[2],c):c,l=a(q[66][61],g);return b(k[72][1],l,f)}return a(k[67][9],d)}function
au(d,c){function
e(e){var
f=P[2];function
g(a){return b(f,0,a)}var
h=i(p[42][1],g,e,c)[1],j=b(E[136],d,c),l=a(k[65][1],h);return b(k[18],l,j)}return a(k[67][9],e)}function
bx(c,b,a){return aA(by[5],[0,ec[116]],0,eb,ea,c,b,a)}function
H(f,e){function
c(c){var
g=a(k[67][4],c),d=bx(g,a(p[42][4],c),f),h=d[1],i=a(e,d[2]),j=a(k[65][1],h);return b(k[18],j,i)}return a(k[67][9],c)}function
u(b){var
c=a(f[6],b);return a(e[8],c)}function
I(j){function
d(i){function
f(a){return b(p[42][7],i,a)}try{var
d=j[3];switch(d[0]){case
0:var
D=a(e[8],d[1]),g=a(E[45],D);break;case
1:var
F=a(e[8],d[1]),w=u(j[1]),G=u(j[2]),J=function(a){return Y(dV,[0,a,G,w,F],E[45])},g=H(f(w),J);break;case
2:var
x=d[1],K=u(x),L=function(a){var
b=E[45];return Y(bo,[0,a,u(x)],b)},g=H(f(K),L);break;case
3:var
y=d[2],o=d[1],M=u(o[1]),z=u(o[2]),N=u(y[2]),O=function(a){var
c=ai(bp,[0,a,M,z,N],2),d=[0,I(y),0],e=[0,I(o),d];return b(q[66][21],c,e)},g=H(f(z),O);break;case
4:var
r=d[2],t=d[1],l=u(t[1]),h=u(r[1]),m=u(t[2]),A=u(r[2]),P=function(g){function
d(j){function
d(d){var
f=a(n[1][6],ed),k=b(p[42][11],f,i),o=[0,a(e[9],1),[0,h]],s=[0,[0,k],g,a(e[21],o)],u=ai(aU,[0,g,d,a(e[19],s),l,m],1),v=ai(aU,[0,j,d,m,h,A],1),w=a(e[21],[0,m,[0,A]]),x=a(e[21],[0,m,[0,h]]),y=ai(bp,[0,d,a(e[21],[0,l,[0,h]]),x,w],2),z=a(c[3],ee),B=[0,b(q[66][5],0,z),0],C=[0,E[122],B],D=I(r),F=[0,b(q[66][3],v,D),C],G=[0,a(q[66][26],F),0],H=I(t),J=[0,b(q[66][3],u,H),G];return b(q[66][21],y,J)}return H(f(a(e[21],[0,l,[0,h]])),d)}return H(f(h),d)},g=H(f(l),P);break;default:var
v=d[1],Q=d[4],R=d[3],S=d[2],B=u(v[1]),T=u(v[2]),C=u(j[1]),U=a(e[9],(1+R|0)-Q|0),V=function(c){function
d(o){var
f=S[1][2],d=a(p[42][4],i),g=a(e[9],1),h=a(p[42][5],i),j=aA(bw[38],h,d,f,g,c,U,C),l=a(n[1][6],d8),m=[0,[0,b(p[42][11],l,i)],c,j],r=ai(aU,[0,c,o,a(e[19],m),B,T],1),s=I(v),t=b(q[66][3],r,s),u=a(k[65][1],d);return b(q[66][3],u,t)}return H(f(C),d)},g=H(f(B),V)}return g}catch(c){c=s(c);if(a(k[71][9],c))return b(k[21],0,c);throw c}}return a(k[67][9],d)}function
eg(c){function
d(d){var
e=P[2];function
f(a){return b(e,0,a)}var
g=i(p[42][1],f,d,c)[1],h=a(E[45],c),j=a(k[65][1],g);return b(k[18],j,h)}return a(k[67][9],d)}function
bz(j,h,f,i){function
c(c){var
g=u(h),d=u(f);function
k(f){var
l=a(n[1][6],eh),h=b(p[42][11],l,c),m=a(n[1][6],ei),o=b(p[42][11],m,c),r=[0,[0,o],f,a(e[9],1)],s=a(e[19],r),t=[0,Y(dQ,[0,f,g,s,j,d,a(e[10],h)],eg),0],k=[0,f,g,d],u=[0,I(i),t],v=[0,h],x=Y(w,k,function(a){return au(v,a)});return b(q[66][21],x,u)}return H(b(p[42][7],c,d),k)}return a(k[67][9],c)}function
bA(Z,X){function
d(h){var
_=a(p[42][4],h);a(aT[3],aT[10]);function
$(b){return a(c[3],el)}a(f[7],$);var
z=a(p[42][5],h),r=a(p[42][4],h),N=[0,a(k[67][13],h),r],d=b(f[11],Z,N),s=[0,0],A=[0,0];function
O(a){var
c=C(z,r,a);b(f[12],d,c);return 0}b(j[17][11],O,X);var
P=a(k[67][3],h);function
Q(i){var
h=a(bv[2][1][1],i),e=a(o[2],h),c=d7(z,r,a(bv[2][1][3],i)),g=c[1];if(aB<=g){if(bQ<=g)return bI<=g?B(f[15],d,h,1,c[2]):B(f[15],d,h,0,c[2]);if(bK<=g){var
k=c[2];return B(f[14],d,[0,e],k[2],k[3])}var
l=c[2];return B(f[13],d,e,l[2],l[3])}if(bV<=g){var
m=c[2],p=s[1],q=function(a){return B(f[14],d,[2,a[1],e],a[2],m)};b(j[17][11],q,p);A[1]=[0,[0,e,m],A[1]];return 0}var
n=c[2],t=A[1];function
u(a){return B(f[14],d,[2,e,a[1]],n,a[2])}b(j[17][11],u,t);s[1]=[0,[0,e,n],s[1]];return 0}b(j[17][11],Q,P);var
D=aV(z,r,a(p[42][6],h));if(aB<=D[1]){var
J=D[2];B(f[14],d,0,J[2],J[3])}else{var
R=D[2],S=s[1],T=function(a){return B(f[14],d,[1,a[1]],a[2],R)};b(j[17][11],T,S)}function
aa(b){return a(c[3],em)}a(f[7],aa);var
K=b(f[29],1,d);function
ab(b){return a(c[3],en)}a(f[7],ab);var
g=a(f[8],d);if(K){var
t=K[1],ac=function(b){return a(c[3],eo)};a(f[7],ac);if(typeof
t==="number"){var
L=a(k[67][4],h),ad=a(f[10],g),ae=function(c){var
h=b(f[21],g,c[1]),i=c[3];function
k(a){return u(b(f[20],g,a))}var
l=b(j[17][14],k,i),d=h[1],m=d[1],n=c[2],o=[0,m,a(e[2][1],d[2])],p=[0,a(e[28],o),l];return[0,a(e[34],p),n]},af=b(j[17][69],ae,ad),ag=b(bB[3],0,ep),ah=function(a){var
c=a[2],d=aA(er[9],0,eq,0,n[1][10][1],L,_,a[1]);function
e(a){return ag}var
f=[4,d,b(j[17][56],c,e)],g=b(bB[3],0,f);return b(aZ[42],L,g)},ai=a(c[3],es);b(aH[6],0,ai);var
aj=a(c[3],et),ak=a(c[3],eu),al=function(h){var
d=a(c[3],ev),e=a(c[13],0),f=a(c[3],ew),g=b(c[12],f,e);return b(c[12],g,d)},am=i(c[39],al,ah,af),an=a(c[3],ex),ao=b(c[12],an,am),ap=b(c[12],ao,ak),aq=b(c[26],8,ap),as=a(c[3],ey),at=b(c[12],as,aq),av=b(c[12],at,aj);b(aH[6],0,av);var
aw=a(c[3],ez);return b(q[66][4],0,aw)}else{if(0===t[0]){var
v=t[1],M=v[2],F=b(ar[13],g,[0,bH,[0,v[1],M,v[3],v[4]]]);b(f[21],g,M[1]);var
W=function(c){var
d=u(F[1]),g=u(F[2]),h=a(k[67][4],c),i=a(p[42][4],c),e=bx(h,i,b(p[42][7],c,d)),j=e[2],l=e[1],m=a(n[1][6],ek),f=b(p[42][11],m,c),r=[0,a(bw[16],f),0],o=[0,j,d,g],s=[0,I(F),r],t=[0,f],y=Y(w,o,function(a){return au(t,a)}),v=b(q[66][21],y,s),x=a(k[65][1],l);return b(q[66][3],x,v)};return a(k[67][9],W)}var
m=t[1],x=b(ar[13],g,[0,-608347012,[0,m[1],m[2]]]),G=b(f[20],g,m[1]),y=b(f[20],g,m[2]),l=m[3];if(typeof
l==="number")return I(x);else
switch(l[0]){case
0:var
ax=a(e[8],l[1]),U=function(c){var
d=u(G),g=u(y),h=a(n[1][6],ef),f=b(p[42][11],h,c),i=[0,ax,[0,a(e[10],f)]],j=a(e[21],i);function
k(c){var
h=[0,a(E[99],j),0],e=[0,c,d,g],i=[0,I(x),h],k=[0,f],l=Y(w,e,function(a){return au(k,a)});return b(q[66][21],l,i)}return H(b(p[42][7],c,d),k)};return a(k[67][9],U);case
1:return bz(a(e[8],l[1]),G,y,x);default:var
ay=l[2],az=a(e[8],l[1]),aC=a(e[8],ay),V=function(d){var
f=u(y),g=a(n[1][6],ej),c=b(p[42][11],g,d),h=[0,aC,[0,a(e[10],c)]],i=a(e[21],h),j=[0,a(E[99],i),0],k=[0,bz(az,G,y,x),j],l=au([0,c],f);return b(q[66][21],l,k)};return a(k[67][9],V)}}}var
aD=a(c[3],eA);return b(q[66][4],0,aD)}return a(k[67][9],d)}var
eC=a(c[3],eB),bC=b(q[66][5],0,eC);function
bD(d,c){var
e=bA(d,c),f=a(q[66][32],E[17]),g=b(q[66][3],f,e);return b(q[66][12],g,bC)}function
eD(c,d,l,j){var
f=T(c);function
g(m){function
c(c){var
n=P[2];function
o(a){return b(n,0,a)}var
f=i(p[42][1],o,c,d),q=f[2],r=f[1],s=a(p[42][5],c),g=aA(by[5],0,0,0,eE,s,r,q),t=g[1],h=a(e[21],[0,m,[0,g[2],d,l]]),u=a(p[42][5],c),v=B(P[2],0,u,t,h)[1],w=a(j,h),x=a(k[65][1],v);return b(k[18],x,w)}return a(k[67][9],c)}var
h=U===f?c[1]:N===f?a(S[2],c):c,m=a(q[66][61],h);return b(k[72][1],m,g)}function
eF(l){var
y=a(k[67][2],l),c=a(p[42][4],l);function
z(d,c){try{var
e=0,f=E[86],g=[0],h=function(a){return Y(bo,g,a)}(f),i=[0,a(q[66][24],h),e],j=[0,a(k[16],0),i],l=eD(w,d,c,E[143]),m=b(q[66][21],l,j);return m}catch(c){c=s(c);if(a(k[71][9],c))return b(k[21],0,c);throw c}}function
A(d){var
c=d[1],e=d[2];if(c[1]!==eG[1])if(c[1]!==eH[1])return b(k[21],[0,e],c);return a(k[16],0)}var
g=b(e[3],c,y);if(9===g[0]){var
h=g[2];if(3===h.length-1){var
n=T(w),B=g[1],C=h[2],D=h[3],F=U===n?w[1]:N===n?a(S[2],w):w;if(i(M[al],c,F,B)){var
o=b(e[3],c,C),t=b(e[3],c,D);if(9===o[0])if(9===t[0]){var
v=t[2],j=o[2];if(j.length-1===v.length-1)var
x=function(c){if(0<=c){var
d=x(c-1|0),e=r(v,c)[c+1],f=z(r(j,c)[c+1],e);return b(q[66][16],f,d)}var
g=bD(aX,0);return a(q[66][24],g)},u=x(j.length-1-1|0),f=1;else
var
f=0}else
var
f=0;else
var
f=0;if(!f)var
u=a(k[16],0);var
m=u,d=1}else
var
d=0}else
var
d=0}else
var
d=0;if(!d)var
m=a(k[16],0);return b(k[23],m,A)}var
Z=[0,I,bA,bC,bD,a(k[67][9],eF)];ak(185,Z,"Cc_plugin.Cctac");a(eI[10],av);var
eJ=0;function
eK(c,a,d){return b(Z[4],c,a)}var
eN=[0,eM,[1,[0,[5,a(ax[16],aw[11])]],eL,0]],eQ=[0,[0,[0,eP,[1,[5,a(ax[16],aw[16])],eO,eN]],eK],eJ];function
eR(a,c){return b(Z[4],aX,a)}var
eV=[0,[0,[0,eU,[0,eT,[1,[0,[5,a(ax[16],aw[11])]],eS,0]]],eR],eQ];function
eW(a,c){return b(Z[4],a,0)}var
eZ=[0,[0,[0,eY,[1,[5,a(ax[16],aw[16])],eX,0]],eW],eV],e1=[0,[0,e0,function(a){return b(Z[4],aX,0)}],eZ];az(bE[10][8],av,e2,0,0,e1);var
e3=0,e5=[0,[0,e4,function(a){return Z[5]}],e3];az(bE[10][8],av,e6,0,0,e5);var
bF=[0,av];ak(190,bF,"Cc_plugin.G_congruence");ak(191,[0,f,ar,Z,bF],"Cc_plugin");return}
