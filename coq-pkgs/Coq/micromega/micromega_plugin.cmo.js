(function(BA){"use strict";var
gM="QMicromega",kW="__varmap",lC="__p",lc="__p%i",lB="__ff",kt="=",gR=">=",cm=" * ",L="micromega",lA="enum_proof\n",e5='command "',kV="( )^2",d9=148,kU="<>",gU="real_nonlinear_prover",lz="(%a)-(%a)",ks=" [*] ",kT=119,la="__wit",lb=" ,",fd=115,kr="Zero",e9=120,fc="Lia",ly="scale_term : not implemented",kS="]",gQ=117,bT=166,kR=" \n",kq="RED",k$='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',fb=128,lx="parse_zop",lw="Zmicromega",kQ="0",lv=")-(",a0=248,gO="%a * %a",bA=164,kp="Sos_Q",ko="Sos_Z",bm=167,lu=">",d8=" + ",A="Coq",kP="LRA_Q",e8=112,k_=")+(",kN="NQA",kO="AProof : ",kn="<linear_prover",kL=105,kM="LRA_R",gT="$i",aj="$t",e7=182,k8="[",k9="CProof : %a",R="Tauto",km=132,kK="%a + %a",k7=" Cannot find witness",kJ="the use of a specialized external tool called csdp. \n\n",k6=157,gL="PsatzZ",lt="Rdefinitions",kI="Timeout",k5="D",k4=" Skipping what remains of this tactic: the complexity of the goal requires ",ls="A(",kG="psatz_Q",kH='" exited ',kl=216,kE="psatz_Z",kF="Rpow_def",kD="nat",k2=154,k3="PsatzQ",lr=205,gK="pure_sos",fa=195,ah="VarMap",kk="%i ",kC="(%a)+(%a)",lp="}",lq="monoid",k=250,k1="PsatzR",kB="buggy certificate",Z="ZMicromega",ki="NRA",kj="C0",bU="plugins/micromega/certificate.ml",kh="compare_num",d=246,lo="Nia",d6=151,ai="Extension: cannot occur",k0=204,e$=113,kg="ZArithRing",e4=208,kA="{",kf="AProof : %a\n",kz="",a1=149,bz="RingMicromega",ke=134,ln=143,kd="C1",gP="real nonlinear prover",gN=100,lm="%a %s %s",ll="=<",kZ="positive",kc="__arith",d7="Reals",ka=", ",gJ="<=",kb="QArith",e3=438,ky=" -> ",lk="psatz_R",gS="[%a]",kx="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",lj="Bad logical fragment",j$=215,Y="plugins/micromega/g_micromega.ml4",gI="plugins/micromega/mfourier.ml",kw=206,li=171,kv=127,kY="CProof : ",e2=196,lh="EnvRing",kX="Refl",j_="t",e6=209,lf="linear prover",lg="Depth",le="Sos_R",e_=114,j9=147,gH="%i",ku=")^(",ld="}\n",I=BA.jsoo_runtime,y=I.caml_check_bound,X=I.caml_equal,j6=I.caml_float_compare,aZ=I.caml_fresh_oo_id,aa=I.caml_int_compare,j8=I.caml_int_of_string,gG=I.caml_lessthan,Bx=I.caml_list_of_js_array,j7=I.caml_ml_string_length,e1=I.caml_mul,b=I.caml_new_string,j=I.caml_obj_tag,aY=I.caml_register_global,w=I.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):I.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):I.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):I.caml_call_gen(a,[b,c,d])}function
G(a,b,c,d,e){return a.length==4?a(b,c,d,e):I.caml_call_gen(a,[b,c,d,e])}function
aX(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):I.caml_call_gen(a,[b,c,d,e,f])}function
C(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):I.caml_call_gen(a,[b,c,d,e,f,g])}function
d5(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):I.caml_call_gen(a,[b,c,d,e,f,g,h])}function
bS(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):I.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Bz(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):I.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
By(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):I.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
v=I.caml_get_global_data(),ej=[0,0,0],Bw=[12,44,[2,0,[11,b(")\n"),0]]],er=[0,0],cX=[0,0,0],i0=[0,b(A),[0,b("Logic"),[0,b("Decidable"),0]]],ge=Bx([[0,b(A),[0,b("Lists"),[0,b("List"),0]]],[0,b(Z),0],[0,b(R),0],[0,b(bz),0],[0,b(lh),0],[0,b(A),[0,b(L),[0,b(Z),0]]],[0,b(A),[0,b(L),[0,b("RMicromega"),0]]],[0,b(A),[0,b(L),[0,b(R),0]]],[0,b(A),[0,b(L),[0,b(bz),0]]],[0,b(A),[0,b(L),[0,b(lh),0]]],[0,b(A),[0,b(kb),[0,b("QArith_base"),0]]],[0,b(A),[0,b(d7),[0,b(lt),0]]],[0,b(A),[0,b(d7),[0,b(kF),0]]],[0,b("LRing_normalise"),0]]),i2=[0,[0,b(A),[0,b("Numbers"),[0,b("BinNums"),0]]],0],i3=[0,[0,b(A),[0,b(d7),[0,b(lt),0]]],[0,[0,b(A),[0,b(d7),[0,b(kF),0]]],[0,[0,b(A),[0,b(d7),[0,b("Raxioms"),0]]],[0,[0,b(A),[0,b(kb),[0,b("Qreals"),0]]],0]]]],i4=[0,[0,b(A),[0,b("ZArith"),[0,b("BinInt"),0]]],0],jU=b(".csdp.cache"),x=b("micromega_plugin"),h=v.Pervasives,f=v.Num,n=v.Printf,q=v.Big_int,E=v.Unix,ee=v.Marshal,ed=v.Printexc,g=v.List,bn=v.Hashtbl,D=v.Assert_failure,gZ=v.Ratio,fe=v.Failure,ec=v.Set,O=v.Not_found,bJ=v.Map,br=v.CErrors,iI=v.String,i=v.CamlinternalLazy,H=v.Names,jH=v.Term,m=v.EConstr,Q=v.Loc,ao=v.Tactics,aV=v.Tacmach,K=v.Tacticals,bw=v.Pp,d2=v.Proofview,jq=v.Invalid_argument,cY=v.Coqlib,gc=v.Goptions,r=v.Ltac_plugin,cl=v.Stdarg,W=v.Genarg,ap=v.Mltop,ay=v.Array,lD=b(kQ),lE=[0,[12,118,[2,0,0]],b("v%s")],lF=[0,[11,b("1/("),[15,[12,41,0]]],b("1/(%a)")],lG=[0,[11,b("- ("),[15,[12,41,0]]],b("- (%a)")],lH=[0,[12,40,[15,[11,b(k_),[15,[12,41,0]]]]],b(kC)],lI=[0,[12,40,[15,[11,b(lv),[15,[12,41,0]]]]],b(lz)],lJ=[0,[12,40,[15,[11,b(")*("),[15,[12,41,0]]]]],b("(%a)*(%a)")],lK=[0,[12,40,[15,[11,b(")/("),[15,[12,41,0]]]]],b("(%a)/(%a)")],lL=[0,[12,40,[15,[11,b(ku),[4,3,0,0,[12,41,0]]]]],b("(%a)^(%i)")],lM=[0,[11,b("Aeq("),[4,3,0,0,[12,41,0]]],b("Aeq(%i)")],lN=[0,[11,b("Ale("),[4,3,0,0,[12,41,0]]],b("Ale(%i)")],lO=[0,[11,b("Alt("),[4,3,0,0,[12,41,0]]],b("Alt(%i)")],lP=[0,[11,b("eq("),[2,0,[12,41,0]]],b("eq(%s)")],lQ=[0,[11,b("le("),[2,0,[12,41,0]]],b("le(%s)")],lR=[0,[11,b("lt("),[2,0,[12,41,0]]],b("lt(%s)")],lS=[0,[12,40,[15,[11,b(")^2"),0]]],b("(%a)^2")],lT=[0,[11,b(lq),0],b(lq)],lU=[0,[15,[11,b(cm),[15,0]]],b(gO)],lV=[0,[15,[11,b(d8),[15,0]]],b(kK)],lW=[0,[15,[11,b(cm),[15,0]]],b(gO)],lZ=b(";"),l7=b("map3"),mA=[0,[11,b(e5),[2,0,[11,b(kH),[4,3,0,0,0]]]],b('command "%s" exited %i')],mz=[0,[11,b(e5),[2,0,[11,b(kH),[2,0,0]]]],b('command "%s" exited %s')],mB=[0,[11,b(e5),[2,0,[11,b('" killed '),[4,3,0,0,0]]]],b('command "%s" killed %i')],mC=[0,[11,b(e5),[2,0,[11,b('" stopped '),[4,3,0,0,0]]]],b('command "%s" stopped %i')],mq=[0,b("plugins/micromega/mutils.ml"),287,7],mk=b("select_pos"),md=[0,0,0],ma=b("list_fold_right_elements"),l_=b("try_find"),l2=b("from_option"),n1=[0,0],oa=[0,0,0],ob=[0,[0,0],0],nV=[0,0],nU=[0,0],nT=[0,0],nL=[0,[0,0]],nM=[0,[0,0]],nN=[0,[0,0]],nO=[0,[0,0]],nH=[0,[0,0]],nI=[0,[0,0]],nJ=[0,[0,0]],nK=[0,[0,0]],nn=[0,[0,0],0],nl=[0,0,0],ne=[0,1],nf=[0,2],ng=[0,3],m$=[0,0],nb=[0,0],na=[0,1],nd=[0,3],nc=[0,0],mR=[0,[1,0]],mS=[0,0,[0,0]],mT=[0,[0,0],0],mU=[0,0],mV=[0,[1,0]],mW=[0,[1,0]],mX=[0,0],mY=[0,[1,0]],mZ=[0,[1,0]],m0=[0,[1,0]],m1=[0,0],m2=[0,[1,0]],m3=[0,0,0],m4=[0,0,0],m5=[0,0],m6=[0,0,0],m7=[0,0],mN=[1,0],mM=[0,0],mE=[1,0],mF=[1,0],mG=[0,0],mI=[0,0],mH=[0,0],ns=[0,0],nF=[0,0],nZ=[0,0],n3=[0,[0,0],0],n4=[0,0,0],n5=[0,[0,0],0],n6=[0,0,0],n7=[0,[0,0],0],n8=[0,0,0],n9=[0,0,0],n_=[0,0,0],oc=[0,[0,0],0],od=[0,0,0],oe=[0,[0,0],0],of=[0,0,0],og=[0,[0,0],0],oh=[0,0,0],oi=[0,0,0],oj=[0,0,0],o5=[0,[11,b(kr),0],b(kr)],o6=[0,[11,b("Hyp "),[4,3,0,0,0]],b("Hyp %i")],o7=[0,[11,b("Def "),[4,3,0,0,0]],b("Def %i")],o8=[0,[11,b("Cst "),[2,0,0]],b("Cst %s")],o9=[0,[11,b(kV),0],b(kV)],o_=[0,[11,b("P * "),[15,0]],b("P * %a")],o$=[0,[12,40,[15,[11,b(")/"),[2,0,0]]]],b("(%a)/%s")],pa=[0,[15,[11,b(cm),[15,0]]],b(gO)],pb=[0,[15,[11,b(d8),[15,0]]],b(kK)],pc=[0,[12,91,[15,[12,93,0]]],b(gS)],pd=[0,[12,46,0],b(".")],pe=[0,[4,3,0,0,[11,b(":= "),[15,[11,b(" ; "),[15,0]]]]],b("%i:= %a ; %a")],pf=[0,[4,3,0,0,[12,123,[15,[11,b(gJ),[15,[11,b(gJ),[15,[12,125,[15,0]]]]]]]]],b("%i{%a<=%a<=%a}%a")],pv=[0,0],pu=[0,[11,b("apply_pivot -> {"),[15,[11,b(ld),0]]],b("apply_pivot -> {%a}\n")],pt=[0,[11,b("xpivot_eq {"),[15,[11,b("} "),[15,[12,32,[2,0,[11,b(" {"),[15,[11,b(ld),0]]]]]]]]],b("xpivot_eq {%a} %a %s {%a}\n")],ps=[0,0],pr=[0,[11,b("mult "),[2,0,[12,32,[15,[11,b(" ("),[15,[12,44,[2,0,[11,b(") -> ("),[15,Bw]]]]]]]]]],b("mult %s %a (%a,%s) -> (%a,%s)\n")],pq=[0,0],pp=[0,0,[0,0]],po=[0,0,[0,0]],pm=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],b(lm)],pl=[0,[2,0,[12,46,[15,[11,b(" +"),0]]]],b("%s.%a +")],pi=[0,b("plugins/micromega/polynomial.ml"),542,10],ph=[0,[11,b("normalise_proof "),[15,[11,b(ky),[15,0]]]],b("normalise_proof %a -> %a")],o2=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],b(lm)],o0=b(gR),oZ=b(kt),oR=b(kh),oS=b(kh),oV=[0,0],oQ=[0,0],oN=[0,0],oL=[0,[2,0,[12,e9,[4,3,0,0,[11,b(d8),0]]]],b("%sx%i + ")],oC=[0,0],oB=[0,1],oz=[0,0],ox=[0,[2,0,[12,32,0]],b("%s ")],oy=[0,[2,0,[12,42,[15,[12,32,0]]]],b("%s*%a ")],on=[0,[12,e9,[4,3,0,0,[12,46,0]]],b("x%i.")],oo=[0,[12,e9,[4,3,0,0,[12,94,[4,3,0,0,[12,46,0]]]]],b("x%i^%i.")],oO=[0,1],pG=[0,[12,72,[4,3,0,0,0]],b("H%i")],pH=[0,[11,b("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],b("E(%i,%a,%a)")],pI=[0,[11,b(ls),[15,[12,44,[15,[12,41,0]]]]],b("A(%a,%a)")],qJ=[0,1],qK=[0,[0,0,0]],qG=b("merge_proof : pivot is not possible"),qH=[0,1],qI=[0,1],qF=[0,0],qz=[0,1],qA=[0,1],qB=[0,1],qC=[0,1],qD=[0,1],qE=[0,1],qv=[0,0],qw=[0,-1],qx=[0,[11,b("optimise Exception : "),[2,0,0]],b("optimise Exception : %s")],qt=[0,0,0],qk=[0,0],ql=[0,0],qm=[0,0],qn=[0,0],qo=[0,0],qp=[0,0],qq=[0,0],qr=[0,0],qj=[0,0],qi=b("bound_of_variable: impossible"),qh=[0,0,0],qf=[0,0,0],qg=b("SystemContradiction"),qe=[0,0],qd=[0,[4,3,0,0,[11,b(ky),[2,0,[12,10,0]]]],b("%i -> %s\n")],qa=[0,0],p$=[0,0,0,0],p9=[0,0],p7=[0,0],p8=[0,0],p_=[0,b(gI),309,4],p6=[0,b(gI),261,9],p3=[0,1],p4=[0,0],p0=[0,b(gI),199,4],pZ=[0,[11,b("(val x = "),[2,0,[11,b(lb),[15,[12,44,[2,0,[12,41,0]]]]]]],b("(val x = %s ,%a,%s)")],pU=[0,[2,0,[11,b(" <= "),0]],b("%s <= ")],pV=[0,[11,b(gJ),[2,0,[12,10,0]]],b("<=%s\n")],pW=b("\n"),pR=[0,[4,3,0,0,[12,32,0]],b(kk)],pQ=b(kA),pS=b(lp),pN=[0,[4,3,0,0,[12,32,0]],b(kk)],pM=b(kA),pO=b(lp),pK=[0,[12,40,[15,[12,44,[15,[12,41,0]]]]],b("(%a,%a)")],pJ=b("oo"),pB=[0,1],pE=b("Mfourier.SystemContradiction"),p2=b("Mfourier.TimeOut"),rA=b(ly),rB=b("scale term: not implemented"),rC=b(ly),rF=[0,0],rG=b("term_to_q_expr: not implemented"),rL=[0,0],rM=b("term_to_z_expr: not implemented"),rS=b("Cuts should already be compiled"),ss=[0,0],st=[0,1],su=[0,0],sv=[0,1],sw=[0,b(bU),1276,1],sj=[0,[11,b(lA),0],b(lA)],sl=[0,b(bU),1175,2],sk=[0,[11,b("Found interval: "),[15,[11,b(" in ["),[2,0,[12,59,[2,0,[11,b("] -> "),0]]]]]]],b("Found interval: %a in [%s;%s] -> ")],sm=[0,0],sn=[0,1],sq=[0,b(bU),1202,2],so=[0,[11,b("xlia:  "),[15,[11,b(kR),0]]],b("xlia:  %a \n")],sp=[0,[11,b("after reduction:  "),[15,[11,b(kR),0]]],b("after reduction:  %a \n")],sh=[0,[11,b("Found a new bound "),[15,0]],b("Found a new bound %a")],sf=[0,1],sg=[0,0,0],si=b("Interval without proof"),se=[0,[11,b("Bad news : loss of completeness "),[15,[12,61,[2,0,0]]]],b("Bad news : loss of completeness %a=%s")],sc=[0,0],sa=[0,1],sb=[0,-1],r_=[0,1],r$=[0,-1],r6=[0,[2,0,[11,b(cm),[2,0,[11,b(d8),[2,0,[11,b(cm),[2,0,[11,b(" = "),[2,0,[12,10,0]]]]]]]]]],b("%s * %s + %s * %s = %s\n")],r1=[0,1],r2=[0,b(bU),817,5],r3=[0,0],rZ=[0,[11,b(kn),0],b(kn)],r0=[0,[12,62,0],b(lu)],rY=b("proof_of_farkas : not enough hyps"),rU=[0,[11,b(kO),[15,[12,10,0]]],b(kf)],rV=[0,[11,b(kY),[15,0]],b(k9)],rT=[0,[11,b("compiled proof "),[15,[12,10,0]]],b("compiled proof %a\n")],rR=[0,[0,0]],rQ=b("id_of_hyp"),rO=[0,0],rP=[0,1],rK=[0,0],rH=[0,1],rI=[0,0],rE=b("bad index"),rz=b("pexpr_of_cstr_compat"),rx=[0,b(bU),512,9],rv=[0,b(bU),493,12],ro=[0,0],rk=b("cannot happen"),rh=[0,[11,b(kO),[15,[12,10,0]]],b(kf)],ri=[0,[11,b(kY),[15,0]],b(k9)],rg=[0,[11,b("raw certificate "),[2,0,0]],b("raw certificate %s")],rb=b("make_certificate(1)"),rc=b("empty_certificate"),q8=b("= 0"),q9=b("<> 0"),q_=b("> 0"),q$=b(">= 0"),q6=[0,1],q5=[0,0],q0=[0,0],q1=[0,[0,0]],q2=[0,0],q4=[0,b(bU),gN,1],q3=[0,0],qZ=[0,[0,0]],qY=[0,[0,0]],qQ=[0,0],qV=[0,[0,0],0],qW=[0,0,0],rd=b("Certificate.Found"),rf=b("Certificate.Strict"),r4=b("Certificate.FoundProof"),r7=b("Certificate.Result"),sF=[0,0,0],sD=[0,0,0],sB=[0,0,[0,5,0]],sE=[0,1,[0,4,[0,5,0]]],sC=[0,1,[0,6,[0,5,0]]],sz=[0,1,[0,6,[0,5,0]]],sx=b("Persistent_cache.PHashtable(Key).InvalidTableFormat"),sy=b("Persistent_cache.PHashtable(Key).UnboundTable"),sS=b("tt"),sT=b("ff"),sU=b("X "),sV=[0,[11,b(ls),[15,[12,41,0]]],b("A(%a)")],sW=[0,[11,b("C("),[15,[12,44,[15,[12,41,0]]]]],b("C(%a,%a)")],sX=[0,[11,b("D("),[15,[12,44,[15,[12,41,0]]]]],b("D(%a,%a)")],sY=[0,[11,b("N("),[15,[12,41,0]]],b("N(%a)")],s0=b(kz),sZ=[0,[11,b("I("),[15,[2,0,[12,44,[15,[12,41,0]]]]]],b("I(%a%s,%a)")],xs=[0,0],xG=b("[]"),xH=[0,[12,91,[15,[12,93,0]]],b(gS)],xI=[0,[12,91,[15,[11,b(ka),[15,[11,b(ka),[15,[12,93,0]]]]]]],b("[%a, %a, %a]")],xK=[0,[12,68,0],b(k5)],xL=[0,[11,b("R["),[15,[12,44,[15,[12,93,0]]]]],b("R[%a,%a]")],xM=[0,[11,b("C["),[15,[12,44,[15,[12,93,0]]]]],b("C[%a,%a]")],xN=b(kS),xO=b(k8),xP=[0,[11,b("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],b("EP[%a,%a,%a]")],x4=b("abstract_wrt_formula"),y4=b(gU),y2=b(gU),yZ=b(gU),yT=b(gP),yS=b(gP),yR=b(gP),yC=b(kB),yB=b(kB),yv=b("csdpcert"),yw=b(L),yx=b("plugins"),yp=b(kc),yq=[0,0],yr=b(k7),yj=b(lj),yk=b(kI),yl=b(kx),ym=b(k$),yn=b(kJ),yo=b(k4),yd=b(la),ye=b(j_),yf=[0,[0,b(A),[0,b(L),[0,b(ah),0]]],[0,[0,b(ah),0],0]],yg=b(ah),yh=b(kW),yi=b(lB),ya=b(kc),yb=[0,0],yc=b(k7),x6=b(lj),x7=b(kI),x8=b(kx),x9=b(k$),x_=b(kJ),x$=b(k4),x2=b("bad old index"),x3=b("proof compaction error"),x0=[0,[15,[11,b(" ;"),0]],b("%a ;")],xZ=b(k8),x1=b(kS),xX=[0,0],xU=b(la),xV=b(kW),xW=b(lB),xJ=[0,[15,[12,47,[15,0]]],b("%a/%a")],xD=b(j_),xE=[0,[0,b(A),[0,b(L),[0,b(ah),0]]],[0,[0,b(ah),0],0]],xF=b(ah),xA=b("Empty"),xB=[0,[0,b(A),[0,b(L),[0,b(ah),0]]],[0,[0,b(ah),0],0]],xC=b(ah),xx=b("Leaf"),xy=[0,[0,b(A),[0,b(L),[0,b(ah),0]]],[0,[0,b(ah),0],0]],xz=b(ah),xu=b("Node"),xv=[0,[0,b(A),[0,b(L),[0,b(ah),0]]],[0,[0,b(ah),0],0]],xw=b(ah),v9=b(kj),v_=b(kd),v$=b("CQ _"),wa=[0,[12,40,[15,[11,b(d8),[15,[12,41,0]]]]],b("(%a + %a)")],wb=[0,[12,40,[15,[11,b(" - "),[15,[12,41,0]]]]],b("(%a - %a)")],wc=[0,[12,40,[15,[11,b(cm),[15,[12,41,0]]]]],b("(%a * %a)")],wd=[0,[11,b("(/ "),[15,[12,41,0]]],b("(/ %a)")],we=[0,[11,b("(- "),[15,[12,41,0]]],b("(- %a)")],w8=[0,0,0],xp=[0,[11,b(lC),[4,3,0,0,0]],b(lc)],xo=[0,[11,b(lC),[4,3,0,0,0]],b(lc)],xn=[0,[11,b("__x"),[4,3,0,0,0]],b("__x%i")],xl=[0,b("plugins/micromega/coq_micromega.ml"),1385,11],w$=b("error : parse_arith(2)"),w_=b("parse_qexpr parse error"),w9=[0,0],wU=b("get_rank"),wR=[1,b("Oups")],wP=b(lx),wO=b(lx),wL=[0,[12,40,[15,[12,32,[15,[12,32,[15,[12,41,0]]]]]]],b("(%a %a %a)")],wE=[0,[12,61,0],b(kt)],wF=[0,[11,b(kU),0],b(kU)],wG=[0,[11,b(ll),0],b(ll)],wH=[0,[11,b(gR),0],b(gR)],wI=[0,[12,60,0],b("<")],wJ=[0,[12,62,0],b(lu)],wx=[0,[12,48,0],b(kQ)],wy=[0,[11,b("(In "),[15,[12,41,[12,37,[11,b(kD),0]]]]],b("(In %a)%%nat")],wz=[0,[12,40,[15,[11,b("^2)"),0]]],b("(%a^2)")],wA=[0,[11,b("( "),[15,[11,b(ks),[15,[12,41,0]]]]],b("( %a [*] %a)")],wB=[0,[12,40,[15,[11,b(ks),[15,[12,41,0]]]]],b("(%a [*] %a)")],wC=[0,[12,40,[15,[11,b(" [+] "),[15,[12,41,0]]]]],b("(%a [+] %a)")],wD=[0,[12,40,[15,[12,41,[12,37,[11,b(kZ),0]]]]],b("(%a)%%positive")],wu=[0,[12,91,[15,[12,93,0]]],b(gS)],wt=[0,[12,40,[15,[12,32,[12,64,[15,[12,41,0]]]]]],b("(%a @%a)")],wp=[0,[11,b("Pc "),[15,0]],b("Pc %a")],wq=[0,[11,b("Pinj("),[15,[12,44,[15,[12,41,0]]]]],b("Pinj(%a,%a)")],wr=[0,[11,b("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],b("PX(%a,%a,%a)")],wj=[0,[11,b("V "),[15,0]],b("V %a")],wk=[0,[12,40,[15,[11,b(k_),[15,[12,41,0]]]]],b(kC)],wl=[0,[12,40,[15,[11,b(lv),[15,[12,41,0]]]]],b(lz)],wm=[0,[15,[11,b("*("),[15,[12,41,0]]]],b("%a*(%a)")],wn=[0,[11,b("-("),[15,[12,41,0]]],b("-(%a)")],wo=[0,[12,40,[15,[11,b(ku),[15,[12,41,0]]]]],b("(%a)^(%a)")],wg=[0,[15,[11,b(lb),[15,0]]],b("%a ,%a")],wh=[0,[15,0],b("%a")],wi=[0,[2,0,[15,[2,0,0]]],b("%s%a%s")],v7=[0,[2,0,0],b("%s")],v5=[0,[4,3,0,0,0],b(gH)],v3=[0,[4,3,0,0,0],b(gH)],v2=[0,[4,3,0,0,0],b(gH)],vY=b("ukn"),vZ=b("BadTerm"),v0=b("Goal"),vT=b("Formula"),vU=[0,[0,b(A),[0,b(L),[0,b(bz),0]]],[0,[0,b(bz),0],0]],vV=b(bz),vQ=b("Build_Formula"),vR=[0,[0,b(A),[0,b(L),[0,b(bz),0]]],[0,[0,b(bz),0],0]],vS=b(bz),vM=b("N_of_Z"),vN=[0,[0,b(A),[0,b("setoid_ring"),[0,b(kg),0]]],0],vO=b(kg),vI=b("ZWitness"),vJ=[0,[0,b(A),[0,b(L),[0,b(Z),0]]],0],vK=b(gM),vE=b("QWitness"),vF=[0,[0,b(A),[0,b(L),[0,b(gM),0]]],0],vG=b(gM),vA=b("BFormula"),vB=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vC=b(Z),vx=b("I"),vy=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vz=b(Z),vu=b("X"),vv=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vw=b(Z),vr=b("A"),vs=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vt=b(Z),vo=b("N"),vp=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vq=b(Z),vl=b(k5),vm=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vn=b(Z),vi=b("Cj"),vj=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vk=b(Z),vf=b("FF"),vg=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],vh=b(Z),vc=b("TT"),vd=[0,[0,b(A),[0,b(L),[0,b(R),0]]],[0,[0,b(R),0],0]],ve=b(Z),u_=b("make_conj"),u$=[0,[0,b(kX),0],0],va=b(lw),u6=b("make_impl"),u7=[0,[0,b(kX),0],0],u8=b(lw),u4=b("coneMember"),u3=b(gL),u2=b("PsatzC"),u1=b("PsatzAdd"),u0=b("PsatzMulC"),uZ=b("PsatzMulE"),uY=b("PsatzSquare"),uX=b("PsatzIn"),uW=b("OpGt"),uV=b("OpGe"),uU=b("OpLt"),uT=b("OpLe"),uS=b("OpNEq"),uR=b("OpEq"),uQ=b("Pinj"),uP=b("Pc"),uO=b("PX"),uN=b("PEpow"),uM=b("PEsub"),uL=b("PEmul"),uK=b("PEopp"),uJ=b("PEadd"),uI=b("PEc"),uH=b("PEX"),uG=b("Q2R"),uF=b("IZR"),uE=b("pow"),uD=b("Rinv"),uB=b("Rdiv"),uA=b("Rmult"),uz=b("Ropp"),uy=b("Rminus"),ux=b("Rplus"),uw=b("Rlt"),uv=b("Rle"),uu=b("Rge"),ut=b("Rgt"),us=b("Qpower"),ur=b("Qmult"),uq=b("Qopp"),up=b("Qminus"),uo=b("Qplus"),un=b("Qeq"),um=b("Qlt"),ul=b("Qle"),uj=b("Qge"),uh=b("Qgt"),ug=b("Z.pow"),uf=b("Z.mul"),ue=b("Z.opp"),ud=b("Z.sub"),uc=b("Z.add"),ub=b("eq"),ua=b("Z.lt"),t$=b("Z.le"),t_=b("Z.ge"),t9=b("Z.gt"),t7=b("EnumProof"),t5=b("CutProof"),t3=b("RatProof"),t1=b("DoneProof"),tZ=b("ZArithProof"),tY=b("R1"),tX=b("R0"),tW=b("COpp"),tV=b("CInv"),tU=b("CMult"),tT=b("CMinus"),tS=b("CPlus"),tR=b("CZ"),tQ=b("CQ"),tP=b(kd),tO=b(kj),tM=b("Rcst"),tL=b("Qmake"),tJ=b("Build_Witness"),tI=b("R"),tH=b("Q"),tG=b("Zneg"),tF=b("Zpos"),tE=b("Z0"),tD=b("Z"),tC=b("xI"),tB=b("xO"),tA=b("xH"),ty=b(kZ),tw=b("option"),tu=b("None"),tt=b("pair"),ts=b("Npos"),tr=b("N0"),tp=b(kD),to=b("S"),tn=b("O"),tl=b("list"),tk=b("nil"),tj=b("cons"),ti=b("False"),th=b("True"),tg=b("iff"),tf=b("not"),te=b("or"),td=b("and"),s2=[0,0,0],sJ=b(kz),sI=[0,[11,b("time "),[2,0,[12,32,[8,0,0,0,[12,10,0]]]]],b("time %s %f\n")],sL=[0,b(fc),[0,b("Enum"),0]],sM=b("Lia Enum"),sO=[0,b("Lra"),[0,b(lg),0]],sQ=[0,b(fc),[0,b(lg),0]],s9=b(Z),s_=b(Z),s$=b(Z),ta=b(Z),tb=b(Z),tc=b(Z),v1=b("Coq_micromega.M.ParseError"),x5=b("Coq_micromega.CsdpNotFound"),ys=b("csdp"),yE=b(".lia.cache"),yG=b(".nia.cache"),yI=b(".nra.cache"),yL=b(lf),yO=b(lf),yQ=b("nra"),yU=b("lia"),yV=b("nlia"),y5=b(gK),y8=b(gK),y$=b(gK),Bt=[0,b(Y),1,0],Br=[0,b(Y),1,0],Bo=[0,b(Y),1,0],Bn=b(aj),Bp=[0,b(kG)],Bq=b(aj),Bs=b(gT),Bu=[0,b(kG)],Bv=b(k3),Bi=b(ai),Bg=b(ai),Bc=[0,b(Y),1,0],Ba=[0,b(Y),1,0],A9=[0,b(Y),1,0],A8=b(aj),A_=[0,b(lk)],A$=b(aj),Bb=b(gT),Bd=[0,b(lk)],Be=b(k1),A3=b(ai),A1=b(ai),AX=[0,b(Y),1,0],AW=b(aj),AY=[0,b("lra_R")],AZ=b(kM),AR=b(ai),AN=[0,b(Y),1,0],AM=b(aj),AO=[0,b("lra_Q")],AP=b(kP),AH=b(ai),AD=[0,b(Y),1,0],AC=b(aj),AE=[0,b("sos_R")],AF=b(le),Ax=b(ai),At=[0,b(Y),1,0],As=b(aj),Au=[0,b("sos_Q")],Av=b(kp),An=b(ai),Aj=[0,b(Y),1,0],Ai=b(aj),Ak=[0,b("sos_Z")],Al=b(ko),Ad=b(ai),z$=[0,b(Y),1,0],z_=b(aj),Aa=[0,b("xnqa")],Ab=b(kN),z5=b(ai),z1=[0,b(Y),1,0],z0=b(aj),z2=[0,b("xnra")],z3=b(ki),zV=b(ai),zR=[0,b(Y),1,0],zQ=b(aj),zS=[0,b("xnlia")],zT=b(lo),zL=b(ai),zH=[0,b(Y),1,0],zG=b(aj),zI=[0,b("xlia")],zJ=b(fc),zB=b(ai),zx=[0,b(Y),1,0],zv=[0,b(Y),1,0],zs=[0,b(Y),1,0],zr=b(aj),zt=[0,b(kE)],zu=b(aj),zw=b(gT),zy=[0,b(kE)],zz=b(gL),zm=b(ai),zk=b(ai),zf=b(kq),zg=b("myred"),zi=b(kq),zp=b(gL),zE=b(fc),zO=b(lo),zY=b(ki),z8=b(kN),Ag=b(ko),Aq=b(kp),AA=b(le),AK=b(kP),AU=b(kM),A6=b(k1),Bl=b(k3),sA=v.End_of_file,yu=v.Coq_config,yy=v.Envars,yz=v.Filename,xS=v.Evd,xT=v.Termops,xe=v.Retyping,xf=v.Sorts,wN=v.Reductionops,s7=v.Universes,yt=v.System;function
ak(d,b){if(typeof
b==="number")return c(h[54],d,lD);else
switch(b[0]){case
0:var
m=a(f[40],b[1]);return c(h[54],d,m);case
1:return e(n[1],d,lE,b[1]);case
2:return G(n[1],d,lF,ak,b[1]);case
3:return G(n[1],d,lG,ak,b[1]);case
4:var
g=b[1];return C(n[1],d,lH,ak,g[1],ak,g[2]);case
5:var
i=b[1];return C(n[1],d,lI,ak,i[1],ak,i[2]);case
6:var
j=b[1];return C(n[1],d,lJ,ak,j[1],ak,j[2]);case
7:var
k=b[1];return C(n[1],d,lK,ak,k[1],ak,k[2]);default:var
l=b[1];return aX(n[1],d,lL,ak,l[1],l[2])}}function
bV(d,b){switch(b[0]){case
0:return e(n[1],d,lM,b[1]);case
1:return e(n[1],d,lN,b[1]);case
2:return e(n[1],d,lO,b[1]);case
3:var
g=a(f[40],b[1]);return e(n[1],d,lP,g);case
4:var
h=a(f[40],b[1]);return e(n[1],d,lQ,h);case
5:var
i=a(f[40],b[1]);return e(n[1],d,lR,i);case
6:return G(n[1],d,lS,ak,b[1]);case
7:return c(n[1],d,lT);case
8:return C(n[1],d,lU,ak,b[1],bV,b[2]);case
9:return C(n[1],d,lV,bV,b[1],bV,b[2]);default:return C(n[1],d,lW,bV,b[1],bV,b[2])}}var
gV=[0,ak,bV];aY(726,gV,"Micromega_plugin.Sos_types");var
lX=0;function
lY(e,b,d){var
a=d;for(;;){if(a){var
f=a[2];c(e,b,a[1]);c(h[54],b,lZ);var
a=f;continue}return 0}}function
gW(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
l0(c,b){return b?[0,a(c,b[1])]:0}function
l1(b){return b?b[1]:a(h[2],l2)}function
l3(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1][1],d);if(c)return c;var
b=f;continue}return 0}}function
l4(e,d){var
b=0,a=d;for(;;){if(a){var
f=a[2];c(e,b,a[1]);var
b=b+1|0,a=f;continue}return 0}}function
l5(h,f){var
b=0,a=f;for(;;){if(a){var
d=a[2],j=a[1],i=function(d){return function(b,a){return[0,c(h,d,a),b]}}(j),b=e(g[20],i,b,d),a=d;continue}return b}}function
l6(f,d){var
b=0,a=d;for(;;){if(a){var
i=a[2],j=a[1],h=function(d){return function(b,a){return[0,c(f,d,a),b]}}(j),b=e(g[20],h,b,a),a=i;continue}return b}}function
gX(f,d,c,b){if(d){if(c)if(b){var
g=b[1],i=c[1],j=d[1],k=gX(f,d[2],c[2],b[2]);return[0,e(f,j,i,g),k]}}else
if(!c)if(!b)return 0;return a(h[1],l7)}function
l8(g,f,e){var
b=f,a=e;for(;;){if(b){var
h=b[2],i=b[1];if(a){var
d=a[2];if(c(g,i,a[1])){var
b=h,a=d;continue}var
a=d;continue}return 0}return 1}}function
l9(c){return function(d){var
b=d;for(;;){if(b){var
e=b[2],f=b[1];try{var
g=a(c,f);return g}catch(a){a=w(a);if(a[1]===fe){var
b=e;continue}throw a}}return a(h[2],l_)}}}function
l$(g,b){function
d(b){if(b){var
e=b[2],f=b[1];return e?c(g,f,d(e)):f}return a(h[1],ma)}return d(b)}function
mb(e,d){var
a=[0,0,d];for(;;){var
b=a[2],c=a[1];if(b<e)return c;var
a=[0,[0,b,c],b-1|0];continue}}function
mc(h,b){function
c(e,b){var
c=e[2],d=e[1];if(d)return[0,d,[0,b,c]];var
f=a(h,b);return f?[0,[0,[0,f[1],b]],c]:[0,d,[0,b,c]]}return e(g[20],c,md,b)}function
gY(d,b){var
a=c(q[17],d,b),e=c(q[15],d,a),f=c(q[15],b,a),g=c(q[10],e,f);return c(q[10],a,g)}function
d_(b){return 2===b[0]?a(gZ[3],b[1]):q[2]}function
d$(b){switch(b[0]){case
0:return a(q[36],b[1]);case
1:return b[1];default:return a(gZ[2],b[1])}}function
g0(d,c){var
b=d,a=c;for(;;){if(a){var
e=a[2],b=gY(b,d_(a[1])),a=e;continue}return b}}function
g1(e,d){var
b=e,a=d;for(;;){if(a){var
f=a[2],g=d$(a[1]),b=c(q[17],b,g),a=f;continue}return b}}function
me(b){var
a=g1(q[1],b);return 0===c(q[23],a,q[1])?q[2]:a}function
mf(a){var
b=g0(q[2],a);function
d(a){var
d=d_(a),e=d$(a),f=c(q[10],e,b);return c(q[15],f,d)}return c(g[17],d,a)}function
ff(e,a){function
b(d,a){if(a){var
f=a[1],g=b(d+1|0,a[2]);return[0,c(e,f,d),g]}return 0}return b(0,a)}function
mg(c,b){var
d=ff(function(d,b){return[0,b,a(c,d)]},b);return a(g[9],d)}function
g2(c,b){var
d=c+a(g[1],b)|0;return[0,ff(function(b,a){return[0,b,a+c|0]},b),d]}function
mh(a){function
b(e,a){if(a){var
c=a[1],f=a[2],g=c[1],d=g2(e,c[2]),h=d[1];return[0,[0,g,h],b(d[2],f)]}return 0}return b(0,a)}function
mi(h,a){function
b(i){var
a=i;for(;;){if(a){var
d=a[2],e=a[1],f=e[2],j=e[1],k=function(a){return a[2]},l=c(g[17],k,f),m=function(a){return c(g[31],a,h)};if(c(g[28],m,l))return[0,[0,j,f],b(d)];var
a=d;continue}return 0}}return b(a)}function
mj(c,b){function
e(i,d,g){var
c=i,b=g;for(;;){if(d){var
j=d[2],k=d[1];if(b){var
f=b[2],l=b[1];if(c===k)return[0,l,e(c+1|0,j,f)];var
c=c+1|0,b=f;continue}return a(h[2],mk)}return 0}}return e(0,c,b)}function
g3(a){return a?g3(a[1])+1|0:0}function
bW(a){return typeof
a==="number"?1:0===a[0]?1+(2*bW(a[1])|0)|0:2*bW(a[1])|0}function
ml(a){return a?bW(a[1]):0}function
fg(a){return typeof
a==="number"?1:0===a[0]?1+(2*fg(a[1])|0)|0:2*fg(a[1])|0}function
mm(a){return typeof
a==="number"?0:0===a[0]?bW(a[1]):-bW(a[1])|0}function
cn(a){if(typeof
a==="number")return q[2];else{if(0===a[0]){var
b=cn(a[1]),d=c(q[11],2,b);return c(q[7],1,d)}var
e=cn(a[1]);return c(q[11],2,e)}}function
ea(b){if(typeof
b==="number")return q[1];else{if(0===b[0])return cn(b[1]);var
c=cn(b[1]);return a(q[3],c)}}function
mn(a){return[1,ea(a)]}var
mo=[0,g3,bW,ml,fg,mm,cn,ea,mn,function(a){var
b=a[1],d=[1,ea([0,a[2]])],e=[1,ea(b)];return c(f[9],e,d)}];function
g4(a){return 0===a?0:[0,g4(a-1|0)]}function
bX(a){return 1===a?0:1===(a&1)?[0,bX(a>>>1|0)]:[1,bX(a>>>1|0)]}function
mp(a){if(0<=a)return 0===a?0:[0,bX(a)];throw[0,D,mq]}function
fh(a){return 1===a?0:1===(a&1)?[0,fh(a>>>1|0)]:[1,fh(a>>>1|0)]}function
mr(a){var
b=aa(a,0);return 0===b?0:1===b?[0,bX(a)]:[1,bX(-a|0)]}function
eb(d){var
f=a(q[36],2);function
b(a){if(c(q[24],a,q[2]))return 0;var
d=c(q[14],a,f),e=d[1];return c(q[24],q[2],d[2])?[0,b(e)]:[1,b(e)]}return b(d)}function
g5(b){var
c=a(q[22],b);return 0===c?0:1===c?[0,eb(b)]:[1,eb(a(q[3],b))]}var
ms=[0,g4,bX,mp,fh,mr,eb,g5,function(a){var
b=eb(d_(a));return[0,g5(d$(a)),b]}];function
mt(d){var
b=d;for(;;){if(b){var
e=b[2],c=a(b[1],0);if(0===c){var
b=e;continue}return c}return 0}}function
mu(g,f,e){var
b=f,a=e;for(;;){if(b){if(a){var
h=a[2],i=b[2],d=c(g,b[1],a[1]);if(0===d){var
b=i,a=h;continue}return d}return 1}return a?-1:0}}var
mv=[0,mt,mu,function(e,d){var
b=d,c=0;for(;;){if(b){var
f=b[2],g=a(e,b[1])^c,b=f,c=g;continue}return c^a(bn[21],0)}}];function
mw(a){return a}function
mx(a){return a+1|0}var
g6=[0,mw,mx,function(d,b){var
e=a(h[21],b);return c(h[54],d,e)},aa],my=a(ec[1],[0,g6[4]]);function
g7(a){for(;;)try{var
d=c(E[15],0,a)[2];return d}catch(a){a=w(a);if(a[1]===E[1]){var
b=a[2];if(typeof
b==="number")if(11===b)continue}throw a}}var
l=[0,lX,lY,gW,l0,l1,l3,l4,l5,l6,gX,l8,l9,l$,mb,mc,gY,d_,d$,g0,g1,me,mf,ff,mg,g2,mh,mi,mj,mo,ms,mv,g6,my,g7,function(b,s,r){var
f=c(E[67],0,0),i=f[2],j=f[1],k=c(E[67],0,0),l=k[2],m=k[1],o=c(E[67],0,0),p=o[2],t=o[1],u=aX(E[69],b,s,j,l,p),q=a(E[31],i);c(h[60],q,r);a(h[51],q);var
d=g7(u);function
v(e){var
b=[0,j,[0,i,[0,m,[0,l,[0,t,[0,p,0]]]]]];function
d(b){try{var
c=a(E[24],b);return c}catch(a){return 0}}return c(g[15],d,b)}return gW(function(p){switch(d[0]){case
0:var
c=d[1];if(0===c){var
f=a(E[30],m);try{var
j=a(ee[3],f);return j}catch(c){c=w(c);var
g=a(ed[1],c),i=e(n[4],mz,b,g);return a(h[2],i)}}var
k=e(n[4],mA,b,c);return a(h[2],k);case
1:var
l=e(n[4],mB,b,d[1]);return a(h[2],l);default:var
o=e(n[4],mC,b,d[1]);return a(h[2],o)}},v)}];aY(737,l,"Micromega_plugin.Mutils");function
bB(a){return 0===a?1:0}function
bC(a,b){if(a){var
c=a[1];return[0,c,bC(a[2],b)]}return b}function
g8(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
fi(b,a){return b?[0,fi(b[1],a)]:a}var
mD=[0];function
a2(a){return typeof
a==="number"?mE:0===a[0]?[1,a2(a[1])]:[0,a[1]]}function
bY(b,a){if(typeof
b==="number")return typeof
a==="number"?mF:0===a[0]?[1,a2(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,a2(c)]:0===a[0]?[1,co(c,a[1])]:[0,bY(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,bY(d,a[1])]:[1,bY(d,a[1])]}}function
co(b,a){if(typeof
b==="number")return typeof
a==="number"?mG:0===a[0]?[0,a2(a[1])]:[1,a2(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,a2(c)]:0===a[0]?[0,co(c,a[1])]:[1,co(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,a2(d)]:0===a[0]?[1,co(d,a[1])]:[0,bY(d,a[1])]}}function
cp(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,cp(a[1])]}function
cq(a){return typeof
a==="number"?0===a?mH:1:[0,[0,a[1]]]}function
cr(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
g9(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,cp(a[1])]]}function
bZ(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?cr(bZ(c,a[1])):cq(bZ(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,cp(d)]:0===a[0]?cq(cs(d,a[1])):cr(bZ(d,a[1]))}}function
cs(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,cp(c)]:0===a[0]?cq(cs(c,a[1])):cr(bZ(c,a[1]))}var
d=b[1];return typeof
a==="number"?g9(d):0===a[0]?cr(cs(d,a[1])):cq(cs(d,a[1]))}}function
fj(c,b){var
a=bZ(c,b);return typeof
a==="number"?0:a[1]}function
fk(b,a){return typeof
b==="number"?a:0===b[0]?bY(a,[1,fk(b[1],a)]):[1,fk(b[1],a)]}function
ct(a){return typeof
a==="number"?mI:0===a[0]?[0,ct(a[1])]:[0,ct(a[1])]}function
g_(h,g,f){var
c=h,b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?c:1;else{if(0===b[0]){var
d=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
b=d,a=a[1];continue}var
c=2,b=d,a=a[1];continue}}var
e=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
c=1,b=e,a=a[1];continue}var
b=e,a=a[1];continue}}}var
mJ=0;function
g$(a,b){return g_(mJ,a,b)}function
fl(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(g$(e,f)){case
0:return b;case
1:var
c=d,a=b,b=fj(f,e);continue;default:var
c=d,b=fj(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,fl(d,g,a[1])]}}}return 0}}function
mK(b,a){var
c=ct(a);return fl(fi(ct(b),c),b,a)}function
ha(a){return a?a2(ha(a[1])):0}var
z=[0,a2,bY,co,cp,cq,cr,g9,bZ,cs,fj,fk,ct,g_,g$,fl,mK,ha],mL=[0,function(b){return b?[0,a(z[17],b[1])]:0}];function
ef(a,d,b){if(typeof
b==="number")return d;else{if(0===b[0]){var
e=ef(a,d,b[1]);return c(a,d,c(a,e,e))}var
f=ef(a,d,b[1]);return c(a,f,f)}}function
hb(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
b0(c,b){if(b){var
d=b[1],e=b0(c,b[2]);return[0,a(c,d),e]}return 0}function
fm(d,b,a){if(a){var
e=a[1];return c(d,e,fm(d,b,a[2]))}return b}function
fn(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
hc(b){return typeof
b==="number"?mM:0===b[0]?[0,[0,b[1]]]:[1,a(z[4],b[1])]}function
hd(b){return typeof
b==="number"?mN:0===b[0]?[0,a(z[4],b[1])]:[1,[0,b[1]]]}function
bD(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?[1,[1,b[1]]]:[1,a(z[4],b[1])];else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?[0,[1,d]]:0===b[0]?fn(bD(d,b[1])):hc(bD(d,b[1]))}var
e=c[1];return typeof
b==="number"?[0,a(z[4],e)]:0===b[0]?hd(bD(e,b[1])):fn(bD(e,b[1]))}}function
bo(b,a){if(typeof
b==="number")return a;else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?b:0===a[0]?[0,c(z[2],d,a[1])]:bD(d,a[1])}var
e=b[1];return typeof
a==="number"?b:0===a[0]?bD(a[1],e):[1,c(z[2],e,a[1])]}}function
bE(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
eg(b,a){return bo(b,bE(a))}function
bF(b,a){if(typeof
b==="number")return 0;else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?0:0===a[0]?[0,c(z[11],d,a[1])]:[1,c(z[11],d,a[1])]}var
e=b[1];return typeof
a==="number"?0:0===a[0]?[1,c(z[11],e,a[1])]:[0,c(z[11],e,a[1])]}}function
cu(b,a){if(typeof
b==="number")return typeof
a==="number"?0:0===a[0]?1:2;else{if(0===b[0]){var
d=b[1];if(typeof
a!=="number"&&0===a[0])return c(z[14],d,a[1]);return 2}var
e=b[1];if(typeof
a!=="number"&&1===a[0])return g8(c(z[14],e,a[1]));return 1}}function
he(b,a){return 2<=cu(b,a)?0:1}function
fo(b,a){return 1===cu(b,a)?1:0}function
mO(b,a){return 2<=cu(b,a)?1:0}function
mP(b,a){return 1===cu(b,a)?a:b}function
eh(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
mQ(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
bG(b,a){if(typeof
b==="number")return he(mR,a)?mS:mT;else{if(0===b[0]){var
e=bG(b[1],a),f=e[1],c=bo(bF(mV,e[2]),mU);if(fo(c,a))return[0,bF(mW,f),c];var
i=eg(c,a);return[0,bo(bF(mY,f),mX),i]}var
g=bG(b[1],a),h=g[1],d=bF(mZ,g[2]);if(fo(d,a))return[0,bF(m0,h),d];var
j=eg(d,a);return[0,bo(bF(m2,h),m1),j]}}function
hf(b,a){if(typeof
b==="number")return m3;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return m4;else{if(0===a[0])return bG(c,a);var
d=bG(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,bE(f),0];var
l=bo(a,e);return[0,bE(bo(f,m5)),l]}}var
g=b[1];if(typeof
a==="number")return m6;else{if(0===a[0]){var
h=bG(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,bE(j),0];var
m=eg(a,i);return[0,bE(bo(j,m7)),m]}var
k=bG(g,[0,a[1]]),n=k[1];return[0,n,bE(k[2])]}}}function
m8(b,a){return hf(b,a)[1]}var
t=[0,fn,hc,hd,bD,bo,bE,eg,bF,cu,he,fo,mO,mP,eh,mQ,bG,hf,m8,function(b,a){if(typeof
b==="number")return eh(a);else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?eh(b):0===a[0]?[0,c(z[16],d,a[1])]:[0,c(z[16],d,a[1])]}var
e=b[1];return typeof
a==="number"?eh(b):0===a[0]?[0,c(z[16],e,a[1])]:[0,c(z[16],e,a[1])]}}];function
az(b,a){return 0===c(t[9],b,a)?1:0}function
m9(a){return[0,a]}function
m_(a){return[0,a]}function
fp(d,f,e){var
b=f,a=e;for(;;)switch(b[0]){case
0:var
g=b[1];return 0===a[0]?c(d,g,a[1]):0;case
1:var
h=b[2],i=b[1];if(1===a[0]){var
j=a[2];if(0===c(z[14],i,a[1])){var
b=h,a=j;continue}return 0}return 0;default:var
k=b[3],l=b[2],m=b[1];if(2===a[0]){var
n=a[3],o=a[1];if(0===c(z[14],l,a[2])){if(fp(d,m,o)){var
b=k,a=n;continue}return 0}return 0}return 0}}function
_(b,a){switch(a[0]){case
0:return a;case
1:var
d=a[2];return[1,c(z[2],b,a[1]),d];default:return[1,b,a]}}function
hg(b,c){return typeof
b==="number"?c:0===b[0]?[1,[1,b[1]],c]:[1,a(z[4],b[1]),c]}function
M(f,e,a,d,b){switch(a[0]){case
0:return c(e,a[1],f)?_(0,b):[2,a,d,b];case
1:return[2,a,d,b];default:var
g=a[2],h=a[1];return fp(e,a[3],[0,f])?[2,h,c(z[2],g,d),b]:[2,a,d,b]}}function
hh(c,b,a){return[2,[0,b],a,[0,c]]}function
hi(b,a){return hh(b,a,0)}function
ae(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1];return[1,d,ae(c,b[2])];default:var
e=b[2],f=b[1],g=ae(c,b[3]);return[2,ae(c,f),e,g]}}function
bp(d,a,b){switch(a[0]){case
0:return[0,c(d,a[1],b)];case
1:var
e=a[1];return[1,e,bp(d,a[2],b)];default:var
f=a[2],g=a[1];return[2,g,f,bp(d,a[3],b)]}}function
b1(d,a,b){switch(a[0]){case
0:return[0,c(d,a[1],b)];case
1:var
e=a[1];return[1,e,b1(d,a[2],b)];default:var
f=a[2],g=a[1];return[2,g,f,b1(d,a[3],b)]}}function
cv(g,f,e,b,d){switch(d[0]){case
0:return _(b,bp(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=c(t[4],m,b);return typeof
h==="number"?_(b,c(f,i,e)):0===h[0]?_(b,c(f,[1,h[1],i],e)):_(m,cv(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
b==="number"?[2,l,k,c(f,j,e)]:0===b[0]?[2,l,k,cv(g,f,e,[1,b[1]],j)]:[2,l,k,cv(g,f,e,a(z[4],b[1]),j)]}}function
cw(h,g,f,e,b,d){switch(d[0]){case
0:var
o=d[1];return _(b,bp(h,ae(g,e),o));case
1:var
j=d[2],n=d[1],i=c(t[4],n,b);return typeof
i==="number"?_(b,c(f,j,e)):0===i[0]?_(b,c(f,[1,i[1],j],e)):_(n,cw(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
b==="number"?[2,m,l,c(f,k,e)]:0===b[0]?[2,m,l,cw(h,g,f,e,[1,b[1]],k)]:[2,m,l,cw(h,g,f,e,a(z[4],b[1]),k)]}}function
fq(f,g,j,d,e,b){switch(b[0]){case
0:return[2,d,e,b];case
1:var
k=b[2],h=b[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,a(z[4],h[1]),k]];default:var
l=b[3],m=b[2],n=b[1],i=c(t[4],m,e);return typeof
i==="number"?M(f,g,c(j,n,d),m,l):0===i[0]?M(f,g,c(j,[2,n,i[1],[0,f]],d),e,l):M(f,g,fq(f,g,j,d,i[1],n),m,l)}}function
fr(g,f,h,k,d,e,b){switch(b[0]){case
0:return[2,ae(f,d),e,b];case
1:var
l=b[2],i=b[1];if(typeof
i==="number")return[2,ae(f,d),e,l];else{if(0===i[0]){var
p=[1,[1,i[1]],l];return[2,ae(f,d),e,p]}var
q=[1,a(z[4],i[1]),l];return[2,ae(f,d),e,q]}default:var
m=b[3],n=b[2],o=b[1],j=c(t[4],n,e);return typeof
j==="number"?M(g,h,c(k,o,d),n,m):0===j[0]?M(g,h,c(k,[2,o,j[1],[0,g]],d),e,m):M(g,h,fr(g,f,h,k,d,j[1],o),n,m)}}function
U(b,e,d,f,g){switch(g[0]){case
0:return bp(e,f,g[1]);case
1:var
q=g[2],r=g[1];return cv(e,function(a,c){return U(b,e,d,a,c)},q,r,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,bp(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,U(b,e,d,m,h)]:0===k[0]?[2,i,j,U(b,e,d,[1,[1,k[1]],m],h)]:[2,i,j,U(b,e,d,[1,a(z[4],k[1]),m],h)];default:var
n=f[3],o=f[2],p=f[1],l=c(t[4],o,j);if(typeof
l==="number"){var
s=U(b,e,d,n,h);return M(b,d,U(b,e,d,p,i),o,s)}else{if(0===l[0]){var
u=l[1],v=U(b,e,d,n,h);return M(b,d,U(b,e,d,[2,p,u,[0,b]],i),j,v)}var
w=l[1],x=U(b,e,d,n,h);return M(b,d,fq(b,d,function(a,c){return U(b,e,d,a,c)},i,w,p),o,x)}}}}function
F(d,f,g,b,e,h,i){switch(i[0]){case
0:return b1(g,h,i[1]);case
1:var
s=i[2],u=i[1];return cw(f,b,function(a,c){return F(d,f,g,b,e,a,c)},s,u,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],w=bp(f,ae(b,j),v);return[2,ae(b,k),l,w];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
x=F(d,f,g,b,e,o,j);return[2,ae(b,k),l,x]}else{if(0===m[0]){var
y=F(d,f,g,b,e,[1,[1,m[1]],o],j);return[2,ae(b,k),l,y]}var
A=F(d,f,g,b,e,[1,a(z[4],m[1]),o],j);return[2,ae(b,k),l,A]}default:var
p=h[3],q=h[2],r=h[1],n=c(t[4],q,l);if(typeof
n==="number"){var
B=F(d,f,g,b,e,p,j);return M(d,e,F(d,f,g,b,e,r,k),q,B)}else{if(0===n[0]){var
C=n[1],D=F(d,f,g,b,e,p,j);return M(d,e,F(d,f,g,b,e,[2,r,C,[0,d]],k),l,D)}var
E=n[1],G=F(d,f,g,b,e,p,j);return M(d,e,fr(d,b,e,function(a,c){return F(d,f,g,b,e,a,c)},k,E,r),q,G)}}}}function
cx(f,e,d,a,b){switch(a[0]){case
0:return[0,c(e,a[1],b)];case
1:var
g=a[1];return _(g,cx(f,e,d,a[2],b));default:var
h=a[2],i=a[1],j=cx(f,e,d,a[3],b);return M(f,d,cx(f,e,d,i,b),h,j)}}function
cy(d,g,f,b,e,a){return c(b,a,d)?[0,d]:c(b,a,g)?e:cx(d,f,b,e,a)}function
a3(f,j,i,e,g,d,b,h){switch(h[0]){case
0:return _(b,cy(f,j,i,e,d,h[1]));case
1:var
l=h[2],p=h[1],k=c(t[4],p,b);return typeof
k==="number"?_(b,c(g,l,d)):0===k[0]?_(b,c(g,[1,k[1],l],d)):_(p,a3(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
b==="number"){var
q=c(g,m,d);return M(f,e,a3(f,j,i,e,g,d,0,o),n,q)}else{if(0===b[0]){var
r=a3(f,j,i,e,g,d,[1,b[1]],m);return M(f,e,a3(f,j,i,e,g,d,b,o),n,r)}var
s=a3(f,j,i,e,g,d,a(z[4],b[1]),m);return M(f,e,a3(f,j,i,e,g,d,b,o),n,s)}}}function
ab(b,e,f,d,c,g,h){switch(h[0]){case
0:return cy(b,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return a3(b,e,d,c,function(a,g){return ab(b,e,f,d,c,a,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return cy(b,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?ab(b,e,f,d,c,l,i):0===j[0]?ab(b,e,f,d,c,[1,[1,j[1]],l],i):ab(b,e,f,d,c,[1,a(z[4],j[1]),l],i);return M(b,c,ab(b,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=ab(b,e,f,d,c,n,i),u=0,v=a3(b,e,d,c,function(a,g){return ab(b,e,f,d,c,a,g)},i,u,p),w=ab(b,e,f,d,c,_(0,n),k),x=ab(b,e,f,d,c,p,k),y=M(b,c,v,o,t);return U(b,f,c,M(b,c,U(b,f,c,M(b,c,x,o,[0,b]),w),m,[0,b]),y)}}}function
cz(a,e,g,f,b,d){switch(d[0]){case
0:var
h=d[1];return[0,c(f,h,h)];case
1:var
l=d[1];return[1,l,cz(a,e,g,f,b,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=ab(a,e,g,f,b,k,_(0,cy(a,e,f,b,i,c(g,e,e)))),n=cz(a,e,g,f,b,i);return M(a,b,U(a,g,b,M(a,b,cz(a,e,g,f,b,k),j,[0,a]),m),j,n)}}function
hj(c,b,a){return hg(a,hi(c,b))}function
cA(h,g,f,e,d,c,n,b,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return a(c,ab(h,g,f,e,d,j,b));else{if(0===i[0]){var
k=i[1];return a(c,ab(h,g,f,e,d,cA(h,g,f,e,d,c,cA(h,g,f,e,d,c,j,b,k),b,k),b))}var
l=i[1],j=cA(h,g,f,e,d,c,j,b,l),i=l;continue}}function
hk(h,a,g,f,e,d,c,b){return b?cA(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
S(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return hj(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=S(a,f,c,g,e,d,b,j[1]);return F(a,c,e,d,b,S(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=S(a,f,c,g,e,d,b,i[1]);return F(a,c,e,d,b,S(a,f,c,g,e,d,b,j),l)}var
k=S(a,f,c,g,e,d,b,i);return U(a,c,b,S(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=S(a,f,c,g,e,d,b,h[2]);return F(a,c,e,d,b,S(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=S(a,f,c,g,e,d,b,h[2]);return ab(a,f,c,g,b,S(a,f,c,g,e,d,b,p),q);case
5:return ae(d,S(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=S(a,f,c,g,e,d,b,h[1]);return hk(a,f,c,g,b,function(a){return a},s,r)}}function
a4(c,b){if(typeof
b==="number")switch(b){case
0:return 0;case
1:return 1;default:return 2}else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1],e=a4(c,b[2]);return[1,a4(c,d),e];case
2:var
f=b[1],g=a4(c,b[2]);return[2,a4(c,f),g];case
3:return[3,a4(c,b[1])];default:var
h=b[1],i=a4(c,b[2]);return[4,a4(c,h),i]}}var
ei=0;function
ek(e,d,b,f){if(f){var
h=f[2],g=f[1],i=c(d,b,g);if(i){if(a(e,i[1]))return 0;var
j=ek(e,d,b,h);return j?[0,[0,g,j[1]]]:0}var
k=ek(e,d,b,h);return k?[0,[0,g,k[1]]]:0}var
l=c(d,b,b);return l?a(e,l[1])?0:[0,[0,b,0]]:[0,[0,b,0]]}function
hl(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=ek(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
hm(e,d,c,a){var
b=0;return fm(function(f,a){var
b=hl(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
cB(d,c,a,b){if(a){var
e=a[2],f=hm(d,c,a[1],b);return bC(cB(d,c,e,b),f)}return ei}function
al(d,c,f,e,p,o){var
b=p,g=o;for(;;)if(typeof
g==="number")switch(g){case
0:return b?ei:ej;case
1:return b?ej:ei;default:return ej}else
switch(g[0]){case
0:var
h=g[1];return b?a(f,h):a(e,h);case
1:var
i=g[2],j=g[1];if(b){var
q=al(d,c,f,e,b,i);return bC(al(d,c,f,e,b,j),q)}var
r=al(d,c,f,e,b,i);return cB(d,c,al(d,c,f,e,b,j),r);case
2:var
k=g[2],l=g[1];if(b){var
s=al(d,c,f,e,b,k);return cB(d,c,al(d,c,f,e,b,l),s)}var
t=al(d,c,f,e,b,k);return bC(al(d,c,f,e,b,l),t);case
3:var
u=g[1],b=bB(b),g=u;continue;default:var
m=g[2],n=g[1];if(b){var
v=al(d,c,f,e,b,m);return cB(d,c,al(d,c,f,e,bB(b),n),v)}var
w=al(d,c,f,e,b,m);return bC(al(d,c,f,e,bB(b),n),w)}}function
hn(f,e,d){var
b=e,a=d;for(;;){if(b){var
g=b[2],h=b[1];if(a){var
i=a[2];if(c(f,h,a[1])){var
b=g,a=i;continue}return 0}return 0}return 1}}function
el(g,f,e,d,c,b,a){return hn(c,al(g,f,e,d,1,b),a)}function
fs(d,b,a){return bB(c(d,b,a))}function
ft(f,e,b,a){var
d=c(e,b,a);return d?fs(f,b,a):d}function
ho(b,a){switch(b){case
0:return m$;case
1:return 1===a?na:0===a?nb:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?nc:nd}}function
hp(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?ne:0;case
2:return 1===a?0:nf;default:return 1===a?0:0===a?ng:[0,a]}}function
em(c,b){return b?a(c,b[1]):0}function
fu(d,b,a){if(b){var
e=b[1];return a?c(d,e,a[1]):0}return 0}function
hq(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,ab(g,f,e,d,c,b,h),0]]:0}function
hr(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=ho(b[2],a[2]);return em(function(a){return[0,[0,ab(g,f,e,d,c,h,i),a]]},j)}function
cC(e,d,c,b,a){var
f=b[1],g=a[1],h=hp(b[2],a[2]);return em(function(a){return[0,[0,U(e,d,c,f,g),a]]},h)}function
bq(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,hb(b[1],g,[0,[0,a],0])];case
1:return[0,[0,cz(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=bq(a,f,d,e,c,h,g,b[2]);return em(function(b){return hq(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=bq(a,f,d,e,c,h,g,b[2]),n=bq(a,f,d,e,c,h,g,l);return fu(function(b,g){return hr(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=bq(a,f,d,e,c,h,g,b[2]),q=bq(a,f,d,e,c,h,g,o);return fu(function(b,e){return cC(a,d,c,b,e)},q,p);default:var
i=b[1];return ft(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
cD(a,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
b=g[1];switch(h){case
0:return fs(d,b,a);case
1:return c(d,b,a);case
2:return c(f,b,a);default:return ft(d,f,b,a)}}return 0}function
en(c,i,h,g,b,a,f,e){var
d=bq(c,i,h,g,b,a,f,e);return d?cD(c,b,a,d[1]):0}function
hs(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=S(e,j,d,i,c,b,a,h[1]),g=S(e,j,d,i,c,b,a,k);switch(l){case
0:var
m=[0,[0,F(e,d,c,b,a,g,f),2],0];return[0,[0,F(e,d,c,b,a,f,g),2],m];case
1:return[0,[0,F(e,d,c,b,a,f,g),0],0];case
2:return[0,[0,F(e,d,c,b,a,f,g),2],0];case
3:return[0,[0,F(e,d,c,b,a,g,f),2],0];case
4:return[0,[0,F(e,d,c,b,a,f,g),3],0];default:return[0,[0,F(e,d,c,b,a,g,f),3],0]}}function
fv(h,g,f,e,d,c,b,a){var
i=hs(h,g,f,e,d,c,b,a);return b0(function(a){return[0,a,0]},i)}function
ht(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=S(e,j,d,i,c,b,a,h[1]),g=S(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,[0,F(e,d,c,b,a,f,g),0],0];case
1:var
m=[0,[0,F(e,d,c,b,a,g,f),2],0];return[0,[0,F(e,d,c,b,a,f,g),2],m];case
2:return[0,[0,F(e,d,c,b,a,g,f),3],0];case
3:return[0,[0,F(e,d,c,b,a,f,g),3],0];case
4:return[0,[0,F(e,d,c,b,a,g,f),2],0];default:return[0,[0,F(e,d,c,b,a,f,g),2],0]}}function
fw(h,g,f,e,d,c,b,a){var
i=ht(h,g,f,e,d,c,b,a);return b0(function(a){return[0,a,0]},i)}function
eo(f,e){var
d=f,b=e;for(;;)switch(b[0]){case
0:return[0,b[1]];case
1:var
g=b[2],d=c(z[2],b[1],d),b=g;continue;default:var
h=b[3],i=b[2],j=b[1],k=eo(a(z[1],d),h);return[2,[4,eo(d,j),[6,[1,d],[0,i]]],k]}}function
nh(a){return eo(0,a)}function
aA(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1]];case
2:var
d=b[1],e=aA(c,b[2]);return[2,aA(c,d),e];case
3:var
f=b[1],g=aA(c,b[2]);return[3,aA(c,f),g];case
4:var
h=b[1],i=aA(c,b[2]);return[4,aA(c,h),i];case
5:return[5,aA(c,b[1])];default:var
j=b[2];return[6,aA(c,b[1]),j]}}function
hu(b,a){var
c=a[2],d=a[1],e=aA(b,a[3]);return[0,aA(b,d),c,e]}function
ni(q,h,f,g,b){if(typeof
b!=="number")switch(b[0]){case
1:var
m=b[1];if(0===m[0]){var
n=m[1];return c(g,q,n)?0:[5,c(f,n,n)]}return[1,m];case
3:var
a=b[2],d=b[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
a==="number"?0:5===a[0]?[3,[5,c(f,a[1],s)],i]:b}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
a==="number"?0:5===a[0]?[3,[5,c(f,a[1],r)],j]:b}return typeof
a==="number"?0:5===a[0]?c(g,h,a[1])?d:[3,d,a]:b;case
5:var
e=d[1];if(typeof
a==="number")return 0;else
switch(a[0]){case
3:var
k=a[2],l=a[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,c(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,c(f,e,k[1])],l];return c(g,h,e)?a:[3,d,a];case
4:return[4,[3,[5,e],a[1]],[3,[5,e],a[2]]];case
5:return[5,c(f,e,a[1])];default:return c(g,h,e)?a:[3,d,a]}default:return typeof
a==="number"?0:5===a[0]?c(g,h,a[1])?d:[3,d,a]:b}case
4:var
o=b[2],p=b[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return b}function
nj(a){return a[1]}function
nk(a){return a[2]}function
aB(b,a){var
d=c(t[8],a[1],[0,b[2]]);return az(c(t[8],b[1],[0,a[2]]),d)}function
cE(b,a){var
d=c(t[8],a[1],[0,b[2]]),e=c(t[8],b[1],[0,a[2]]);return c(t[10],e,d)}function
aC(b,a){var
d=c(z[11],b[2],a[2]),e=c(t[8],a[1],[0,b[2]]),f=c(t[8],b[1],[0,a[2]]);return[0,c(t[5],f,e),d]}function
a5(b,a){var
d=c(z[11],b[2],a[2]);return[0,c(t[8],b[1],a[1]),d]}function
bH(b){var
c=b[2];return[0,a(t[6],b[1]),c]}function
b2(b,a){return aC(b,bH(a))}function
fx(b){var
a=b[1];return typeof
a==="number"?nl:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
fy(a,b){return ef(a5,a,b)}function
nm(b,a){return typeof
a==="number"?nn:0===a[0]?fy(b,a[1]):fx(fy(b,a[1]))}function
no(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
b3(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,b3(b,a[1],c)]:[1,b3(b,a[1],c),b,0]}function
fz(d,a,b,c){if(typeof
c==="number")return b3(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,b3(d,a[1],b)]:[1,b3(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,fz(d,a[1],b,e)]:[1,fz(d,a[1],b,f),h,e]}}var
np=t[10],nq=t[8],nr=t[5],nt=0;function
nu(a,b){return en(nt,ns,nr,nq,az,np,a,b)}var
nv=t[6],nw=t[7],nx=t[5],ny=0;function
ac(a,b){return F(ny,nx,nw,nv,az,a,b)}var
nz=t[5],nA=0;function
aD(a,b){return U(nA,nz,az,a,b)}var
nB=t[6],nC=t[7],nD=t[8],nE=t[5],nG=0;function
cF(a){return S(nG,nF,nE,nD,nC,nB,az,a)}function
hv(c){var
d=c[3],e=c[2],a=cF(c[1]),b=cF(d);switch(e){case
0:var
f=[0,[0,ac(b,aD(a,nH)),3],0];return[0,[0,ac(a,aD(b,nI)),3],f];case
1:return[0,[0,ac(a,b),0],0];case
2:return[0,[0,ac(a,aD(b,nJ)),3],0];case
3:return[0,[0,ac(b,aD(a,nK)),3],0];case
4:return[0,[0,ac(a,b),3],0];default:return[0,[0,ac(b,a),3],0]}}function
hw(a){var
b=hv(a);return b0(function(a){return[0,a,0]},b)}function
hx(c){var
d=c[3],e=c[2],a=cF(c[1]),b=cF(d);switch(e){case
0:return[0,[0,ac(a,b),0],0];case
1:var
f=[0,[0,ac(b,aD(a,nL)),3],0];return[0,[0,ac(a,aD(b,nM)),3],f];case
2:return[0,[0,ac(b,a),3],0];case
3:return[0,[0,ac(a,b),3],0];case
4:return[0,[0,ac(b,aD(a,nN)),3],0];default:return[0,[0,ac(a,aD(b,nO)),3],0]}}function
hy(a){var
b=hx(a);return b0(function(a){return[0,a,0]},b)}var
nP=t[10],nQ=0;function
fA(a){return cD(nQ,az,nP,a)}var
nR=t[5],nS=0;function
hz(a,b){return cC(nS,nR,az,a,b)}function
hA(e,d){var
a=c(t[17],e,d),b=a[1];return typeof
a[2]==="number"?b:c(t[5],b,nT)}function
fB(b,a){var
d=c(t[19],b,a);return c(t[13],d,nU)}function
cG(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=cG(a[1]),f=b[2],g=b[1],c=cG(e),h=c[2],i=c[1];return[0,fB(fB(g,f),i),h]}}function
cH(a,b){switch(a[0]){case
0:return[0,c(t[18],a[1],b)];case
1:var
d=a[1];return[1,d,cH(a[2],b)];default:var
e=a[2],f=a[1],g=cH(a[3],b);return[2,cH(f,b),e,g]}}function
ep(b){var
e=cG(b),f=e[2],d=e[1];if(c(t[12],d,0)){var
g=hA(a(t[6],f),d),h=a(t[6],g);return[0,cH(b1(t[7],b,f),d),h]}return[0,b,0]}function
eq(d){var
e=d[2],a=d[1];switch(e){case
0:var
f=cG(a),g=f[2],b=f[1];if(c(t[12],b,0))if(bB(az(g,0)))if(bB(az(c(t[19],b,g),b)))return 0;return[0,[0,ep(a),0]];case
1:return[0,[0,[0,a,0],e]];case
2:return[0,[0,ep(b1(t[7],a,nV)),3]];default:return[0,[0,ep(a),3]]}}function
hB(a){var
b=a[1],c=a[2];return[0,aD(b[1],[0,b[2]]),c]}function
hC(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
nW=t[10],nX=t[8],nY=t[5],n0=0;function
cI(a,b){return bq(n0,nZ,nY,nX,az,nW,a,b)}function
fC(a){return 0===a?1:3<=a?1:0}function
fD(w,v){var
d=w,b=v;for(;;)if(typeof
b==="number")return 0;else
switch(b[0]){case
0:var
x=b[2],g=cI(d,b[1]);if(g){var
h=g[1];if(fA(h))return 1;var
d=[0,h,d],b=x;continue}return 0;case
1:var
y=b[2],i=cI(d,b[1]);if(i){var
j=eq(i[1]);if(j){var
d=[0,hB(j[1]),d],b=y;continue}return 1}return 0;default:var
z=b[3],A=b[2],k=cI(d,b[1]);if(k){var
B=k[1],l=cI(d,A);if(l){var
C=l[1],m=eq(B);if(m){var
n=m[1],o=n[1],p=o[1],D=n[2],E=o[2],q=eq(C);if(q){var
r=q[1],s=r[1],F=r[2],G=s[2],H=s[1];if(fC(D))if(fC(F))if(hC(aD(p,H))){var
f=z,e=a(t[6],E);for(;;){if(f){var
I=f[2],J=f[1],u=fD([0,[0,ac(p,[0,e]),0],d],J);if(u){var
f=I,e=c(t[5],e,n1);continue}return u}return c(t[12],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
n2(b,a){return el(fA,hz,hw,hy,fD,b,a)}function
hD(a,b){return en(n4,n3,aC,a5,aB,cE,a,b)}function
hE(a){return fv(n6,n5,aC,a5,b2,bH,aB,a)}function
hF(a){return fw(n8,n7,aC,a5,b2,bH,aB,a)}function
hG(a){return cD(n9,aB,cE,a)}function
hH(a,b){return cC(n_,aC,aB,a,b)}function
n$(b,a){return el(hG,hH,hE,hF,hD,b,a)}function
aJ(a){if(typeof
a==="number")return 0===a?oa:ob;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aJ(a[2]);return aC(aJ(b),c);case
3:var
d=a[1],e=aJ(a[2]);return b2(aJ(d),e);case
4:var
f=a[1],g=aJ(a[2]);return a5(aJ(f),g);case
5:return fx(aJ(a[1]));default:return bH(aJ(a[1]))}}function
hI(a,b){return en(od,oc,aC,a5,aB,cE,a,b)}function
hJ(a){return fv(of,oe,aC,a5,b2,bH,aB,a)}function
hK(a){return fw(oh,og,aC,a5,b2,bH,aB,a)}function
hL(a){return cD(oi,aB,cE,a)}function
hM(a,b){return cC(oj,aC,aB,a,b)}var
s=[0,bB,bC,g8,fi,mD,z,mL,ef,hb,b0,fm,t,az,m9,m_,fp,_,hg,M,hh,hi,ae,bp,b1,cv,cw,fq,fr,U,F,cx,cy,a3,ab,cz,hj,cA,hk,S,a4,ei,ej,ek,hl,hm,cB,bC,al,hn,el,fs,ft,ho,hp,em,fu,hq,hr,cC,bq,cD,en,S,F,U,hs,fv,ht,fw,eo,nh,aA,hu,ni,nj,nk,aB,cE,aC,a5,bH,b2,fx,fy,nm,no,b3,fz,nu,ac,aD,cF,hv,hw,hx,hy,fA,hz,hA,fB,cG,cH,ep,eq,hB,hC,cI,fC,fD,n2,hD,hE,hF,hG,hH,n$,aJ,hI,hJ,hK,hL,hM,function(b,a){return el(hL,hM,hJ,hK,hI,a4(function(a){return hu(aJ,a)},b),a)}];aY(738,s,"Micromega_plugin.Micromega");var
fE=f[2],bI=f[7],ol=f[3],T=a(bJ[1],[0,aa]),ok=0;function
om(a,b){function
d(c,b){return 1===b?e(n[1],a,on,c):G(n[1],a,oo,c,b)}return c(T[12],d,b)}var
fF=T[1];function
hN(a){var
b=0;function
c(c,b,a){return a+b|0}return e(T[13],c,a,b)}function
op(b,a){var
c=hN(b),d=hN(a);return c===d?e(T[10],aa,b,a):aa(c,d)}function
hO(a){return X(a,T[1])}function
oq(a){return e(T[4],a,1,T[1])}function
or(a){try{var
b=1,c=function(c,b,a){if(1===a){if(1===b)return 0;throw O}throw O},d=1-e(T[13],c,a,b);return d}catch(a){a=w(a);if(a===O)return 0;throw a}}function
os(a){if(hO(a))return 0;try{var
b=function(c,b,f){var
d=b/2|0;if(0===(b%2|0))return e(T[4],c,d,a);throw O},c=[0,e(T[13],b,a,fF)];return c}catch(a){a=w(a);if(a===O)return 0;throw a}}function
cJ(b,a){try{var
d=c(T[27],b,a);return d}catch(a){a=w(a);if(a===O)return 0;throw a}}function
ot(b,a){var
c=cJ(b,a)+1|0;return e(T[4],b,c,a)}function
hP(b,a){function
c(b,c,a){var
d=cJ(b,a)+c|0;return e(T[4],b,d,a)}return e(T[13],c,b,a)}function
ou(d,c){var
b=fF,a=c;for(;;){if(0===a)return b;var
b=hP(b,d),a=a-1|0;continue}}function
ov(b,a){var
f=h[7];function
g(e,d,a){var
f=I.caml_div(cJ(e,b),d);return c(h[4],a,f)}var
d=e(T[13],g,a,f),i=T[1];function
j(c,g,b){var
f=g-e1(cJ(c,a),d)|0;return 0===f?b:e(T[4],c,f,b)}return[0,e(T[13],j,b,i),d]}var
J=[0,fF,hO,oq,or,cJ,ot,hP,ou,ov,op,om,T[13],os],V=a(bJ[1],[0,J[10]]);function
ow(b,d){function
g(g,d){if(0===c(J[10],J[1],g)){var
h=a(f[40],d);return e(n[1],b,ox,h)}var
i=J[11],j=a(f[40],d);return aX(n[1],b,oy,j,i,g)}return c(V[12],g,d)}function
hQ(b,a){try{var
d=c(V[27],b,a);return d}catch(a){a=w(a);if(a===O)return oz;throw a}}function
oA(b){var
c=V[1],d=a(J[3],b);return e(V[4],d,oB,c)}function
hR(a){return e(V[4],J[1],a,V[1])}function
hS(d,g,b){if(0===a(f[25],g))return b;var
h=c(fE,hQ(d,b),g);return 0===a(f[25],h)?c(V[7],d,b):e(V[4],d,h,b)}function
hT(g,b,d){if(0===a(f[25],b))return hR(oC);var
h=V[1];function
i(f,d,a){var
h=c(bI,b,d),i=c(J[7],g,f);return e(V[4],i,h,a)}return e(V[13],i,d,h)}function
hU(b,a){function
c(c,b,a){return hS(c,b,a)}return e(V[13],c,b,a)}function
oD(b,a){var
c=V[1];function
d(d,c,b){return hU(hT(d,c,a),b)}return e(V[13],d,b,c)}function
oE(b){function
d(b){return a(f[3],b)}return c(V[33],d,b)}var
hV=V[13];function
oF(b){var
c=1;return e(hV,function(e,c,b){var
d=b?0===a(f[25],c)?1:0:b;return d},b,c)}var
oG=a(V[10],f[37]),hW=[0,hQ,oA,hS,hR,hT,oD,hU,oE,hV,ow,oG,oF,function(b){var
c=1;function
d(c,f,b){if(b){var
d=a(J[2],c);if(!d)return a(J[4],c);var
e=d}else
var
e=b;return e}return e(V[13],d,b,c)}];function
oH(k,j){var
b=k,a=j;for(;;){if(b){var
d=b[1],l=b[2],m=d[2],n=d[1];if(a){var
e=a[1],g=n===e[1]?1:0,o=a[2],p=e[2];if(g){var
h=c(f[26],m,p);if(h){var
b=l,a=o;continue}var
i=h}else
var
i=g;return i}return 0}return a?0:1}}function
oI(e){var
c=0,b=e;for(;;){if(b){var
d=b[1],g=b[2],h=d[1],i=[0,h,a(f[56],d[2])],c=c+a(bn[21],i)|0,b=g;continue}return a(bn[21],c)}}var
oJ=0;function
oK(h,b){function
d(b){var
c=b[1],d=a(f[40],b[2]);return e(n[2],oL,d,c)}return c(g[15],d,b)}function
oM(a){function
d(i,h){var
b=i,a=h;for(;;){if(a){var
e=a[2],g=a[1];if(c(f[31],g,oN))return[0,[0,b,g],d(b+1|0,e)];var
b=b+1|0,a=e;continue}return 0}}return d(0,a)}function
oP(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,er,b(c+1|0,a)]}return 0}return b(0,a)}function
bK(d,b,a){return c(f[26],b,oQ)?a:[0,[0,d,b],a]}function
hX(d,c,b){if(b){var
f=b[2],g=b[1],i=g[2],e=g[1],j=aa(d,e)+1|0;if(2<j>>>0)return a(h[2],oR);switch(j){case
0:return bK(d,a(c,er),b);case
1:return bK(e,a(c,i),f);default:return[0,[0,e,i],hX(d,c,f)]}}return bK(d,a(c,er),0)}function
hY(d,c,b){if(b){var
f=b[2],g=b[1],e=g[1],i=aa(d,e)+1|0,j=g[2];if(2<i>>>0)return a(h[2],oS);switch(i){case
0:return bK(d,c,b);case
1:return bK(e,c,f);default:return[0,[0,e,j],hY(d,c,f)]}}return bK(d,c,0)}function
oT(d){var
f=q[1];function
h(d,b){var
e=a(l[18],b[2]);return c(q[17],d,e)}var
b=e(g[20],h,f,d);return 0===c(q[23],b,q[1])?q[2]:b}function
oU(a,b){if(0===a[0]){var
d=a[1];if(0===d)return 0;if(1===d)return b}function
e(b){var
d=b[1];return[0,d,c(f[7],a,b[2])]}return c(g[17],e,b)}function
es(o,n){var
b=o,a=n;for(;;){if(b){if(a){var
e=a[2],i=a[1],j=i[2],g=i[1],h=b[2],k=b[1],l=k[2],d=k[1];if(X(d,g)){var
m=c(f[1],l,j);if(c(f[26],m,oV)){var
b=h,a=e;continue}return[0,[0,d,m],es(h,e)]}return gG(d,g)?[0,[0,d,l],es(h,a)]:[0,[0,g,j],es(b,e)]}return b}return a?a:0}}function
oW(d,b){var
e=0,g=[0,function(a){return c(f[37],d[2],b[2])},e],h=[0,function(a){return aa(d[1],b[1])},g];return a(l[31][1],h)}var
oX=a(l[31][2],oW);function
hZ(e,d){var
a=d;for(;;){if(a){var
b=a[1],c=aa(b[1],e),f=a[2],g=b[2];if(-1===c){var
a=f;continue}return 0===c?[0,[0,g,a]]:0}return 0}}function
oY(c,b){var
a=hZ(c,b);return a?[0,a[1][1]]:0}var
bL=[0,oH,oI,oJ,oK,oM,er,oO,oP,bK,hX,hY,oT,oU,es,oX,hZ,oY,function(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}];function
fG(a){return 0===a?oZ:o0}function
o1(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fG(d);return C(n[1],c,o2,bL[4],e,h,g)}function
o3(b,a){if(0===b){if(0===a)return 0}else
if(0!==a)return 1;return 1}function
o4(b,a){if(0!==b)if(0!==a)return 1;return 0}function
aE(d,b){if(typeof
b==="number")return c(n[1],d,o5);else
switch(b[0]){case
0:return e(n[1],d,o6,b[1]);case
1:return e(n[1],d,o7,b[1]);case
2:var
f=a(q[33],b[1]);return e(n[1],d,o8,f);case
3:return c(n[1],d,o9);case
4:return G(n[1],d,o_,aE,b[2]);case
5:var
g=b[2],h=a(q[33],b[1]);return aX(n[1],d,o$,aE,g,h);case
6:return C(n[1],d,pa,aE,b[1],aE,b[2]);case
7:return C(n[1],d,pb,aE,b[1],aE,b[2]);default:return G(n[1],d,pc,aE,b[1])}}function
cK(d,b){if(typeof
b==="number")return c(n[1],d,pd);else{if(0===b[0])return d5(n[1],d,pe,b[1],aE,b[2],cK,b[3]);var
e=b[5],f=b[4],g=b[3],h=b[2],i=b[1],j=a(l[2],cK);return By(n[1],d,pf,i,aE,h,bL[4],g,aE,f,j,e)}}function
b4(e){var
a=e;for(;;){if(typeof
a==="number")var
b=0;else
switch(a[0]){case
8:var
d=a[1],b=1;break;case
0:case
1:return a[1];case
4:case
5:var
d=a[2],b=1;break;case
6:case
7:var
f=a[1],g=b4(a[2]),i=b4(f);return c(h[5],i,g);default:var
b=0}if(b){var
a=d;continue}return-1}}function
fH(a){if(typeof
a==="number")return-1;else{if(0===a[0]){var
b=a[2],d=a[1],f=fH(a[3]),i=b4(b),j=c(h[5],i,f);return c(h[5],d,j)}var
k=a[5],l=a[2],m=a[1],n=b4(a[4]),o=b4(l),p=c(h[5],o,n),q=c(h[5],m,p),r=function(b,a){var
d=fH(a);return c(h[5],b,d)};return e(g[20],r,q,k)}}function
a6(b,a){if(typeof
a!=="number")switch(a[0]){case
4:var
n=a[1],d=a6(b,a[2]);return[0,d[1],d[2],[4,n,d[3]]];case
5:var
e=a6(b,a[2]),f=e[2];return[0,[0,[0,f,e[3]],e[1]],f+1|0,[1,f]];case
6:var
o=a[2],g=a6(b,a[1]),p=g[3],q=g[1],i=a6(g[2],o),r=i[2],s=[6,p,i[3]];return[0,c(h[25],i[1],q),r,s];case
7:var
t=a[2],j=a6(b,a[1]),u=j[3],v=j[1],k=a6(j[2],t),w=k[2],x=[7,u,k[3]];return[0,c(h[25],k[1],v),w,x];case
8:var
l=a6(b,a[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[1,m]]}return[0,0,b,a]}function
et(c,a){if(typeof
a!=="number"&&8===a[0]){var
b=a6(c,a[1]);return[0,b[1],b[2],[8,b[3]]]}return a6(c,a)}function
fI(b){var
a=b;for(;;){if(typeof
a!=="number"&&8===a[0]){var
a=a[1];continue}return a}}function
fJ(f,o){var
b=o;for(;;)if(typeof
b==="number")return[0,f,0];else{if(0===b[0]){var
d=b[2],l=b[1];if(typeof
d!=="number"&&5===d[0])if(typeof
b[3]==="number"){var
b=[0,l,d[2],0];continue}var
p=b[3],i=et(f,d),q=i[3],r=i[1],m=fJ(i[2],p),s=m[1],t=[0,l,q,m[2]],u=function(b,a){return[0,a[1],[8,a[2]],b]};return[0,s,e(g[20],u,t,r)]}var
v=b[5],w=b[4],x=b[3],y=b[1],j=et(f,fI(b[2])),z=j[3],A=j[2],B=j[1],k=et(A,fI(w)),C=k[3],D=k[2],E=k[1],F=function(a){return fJ(D,a)},G=c(g[17],F,v),n=a(g[46],G),H=n[2],I=n[1],J=c(h[25],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[8,a[2]],b]},M=e(g[20],L,K,J);return[0,e(g[20],h[5],0,I),M]}}function
pg(c,a){var
b=fJ(c,a);if(l[1])aX(n[2],ph,cK,a,cK,b[2]);return b}function
h0(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[7,b,a];var
c=b}return c}function
fK(b,d){var
e=a(q[22],b)+1|0;if(2<e>>>0)throw[0,D,pi];switch(e){case
0:return[4,[0,0,[1,b]],d];case
1:return 0;default:return c(q[24],b,q[2])?d:[6,[2,b],d]}}function
h1(c,b){var
d=c[2],e=c[1];return e?[4,[0,e,d],b]:fK(a(l[18],d),b)}var
cL=a(bJ[1],[0,J[10]]),cM=a(bJ[1],[0,aa]),cN=[0,cL[1]],cO=[0,cM[1]],eu=[0,0];function
pj(a){cN[1]=cL[1];cO[1]=cM[1];eu[1]=0;return 0}function
pk(b){try{var
a=c(cL[27],b,cN[1]);return a}catch(a){a=w(a);if(a===O){var
d=eu[1];cN[1]=e(cL[4],b,d,cN[1]);cO[1]=e(cM[4],d,b,cO[1]);eu[1]++;return d}throw a}}var
aK=[0,cL,cM,cN,cO,eu,pj,pk,function(a){return c(cM[27],a,cO[1])}];function
fL(a){var
b=a[2],d=a[1];function
e(b,a){return aa(b[1],a[1])}return[0,c(g[48],e,d),b]}function
ev(c,b){var
d=b[2],e=a(aK[8],b[1]),g=J[11],h=a(f[40],d);return aX(n[1],c,pl,h,g,e)}function
ew(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fG(d),i=a(l[2],ev);return C(n[1],c,pm,i,e,h,g)}function
pn(c){function
d(d,c,b){var
e=b[1],f=b[2];return a(J[2],d)?[0,e,c]:[0,[0,[0,a(aK[7],d),c],e],f]}var
b=e(hW[9],d,c,po);return fL([0,b[1],b[2]])}function
h2(b,d,m){var
e=m[2],j=m[1];if(a(J[2],d))var
q=c(bI,b,e),i=[0,c(bL[13],b,j),q];else
if(0===a(f[25],b))var
i=pp;else{if(0===a(f[25],e))var
k=0;else
var
t=c(bI,b,e),k=[0,[0,a(aK[7],d),t],0];var
r=function(e){var
f=e[2],g=a(aK[8],e[1]),h=c(J[7],d,g),i=a(aK[7],h);return[0,i,c(bI,b,f)]},s=c(g[17],r,j),i=fL([0,c(h[25],k,s),pq])}var
o=i[2],p=i[1];if(l[1]){var
u=a(f[40],o),v=a(l[2],ev),w=a(f[40],e),x=a(l[2],ev),y=J[11],z=a(f[40],b);Bz(n[2],pr,z,y,d,x,j,w,v,p,u)}return[0,p,o]}function
h3(c,b){return a(J[2],b)?[0,0,c]:[0,[0,[0,a(aK[7],b),c],0],ps]}function
h4(w,v,d,u){var
i=u[1],j=w[1],F=u[2],G=w[2];if(l[1]){var
H=a(f[40],d),I=a(aK[8],v);bS(n[2],pt,ew,j,J[11],I,H,ew,i)}var
K=i[2],x=a(aK[8],v);function
C(b,a){var
d=a[2],e=b[2],f=a[1],g=b[1];return gG(e,d)?-1:X(e,d)?c(J[10],g,f):1}var
p=[0,J[1],h[7]],o=[0,i,F];for(;;){var
q=o[2],b=o[1],_=p[2],$=p[1],U=b[1],V=[0,pv,J[1],0],W=function(b,d){var
e=b[3],f=b[2],j=d[2],k=b[1],l=a(aK[8],d[1]),g=c(J[9],l,x),h=g[2],i=g[1];return-1===C([0,f,e],[0,i,h])?[0,j,i,h]:[0,k,f,e]},m=e(g[20],W,V,U),D=m[3],Y=m[2],Z=m[1],E=0<D?[0,[0,Z,Y,D]]:0;if(E){var
r=E[1],s=r[3],t=r[2],aa=r[1];if(-1===C([0,t,s],[0,$,_])){var
k=a(f[15],d),y=c(bI,[0,-a(f[25],d)|0],aa),L=c(J[8],x,s-1|0),z=c(J[7],t,L),M=a(f[3],j[3]),A=h2(y,z,[0,j[1],M]),N=A[2],O=A[1],P=c(bI,k,b[3]),Q=c(fE,a(f[3],N),P),R=c(bL[13],k,b[1]),B=[0,c(bL[14],R,O),K,Q],S=fK(a(l[18],k),q),T=h0(h1(h3(y,z),G),S);if(l[1])e(n[2],pu,ew,B);var
p=[0,t,s],o=[0,B,T];continue}return[0,b,q]}return[0,b,q]}}var
p=[0,ok,fE,ol,bI,J,hW,bL,fG,o1,o3,o4,aE,cK,b4,fH,et,fI,pg,h0,fK,h1,[0,aK,fL,ev,ew,pn,h2,h3,h4,function(b,a){var
d=a[1],f=a[2],e=c(bL[17],b,d[1]);if(e){var
g=e[1];return function(a){return[0,h4([0,d,f],b,g,a)]}}return function(a){return 0}}]];aY(742,p,"Micromega_plugin.Polynomial");var
aq=l[4],ex=l[5],pw=0,px=0,py=j6;function
h5(a){var
b=a[1];if(b){var
d=a[2];if(d)return c(f[29],b[1],d[1])?[0,a]:0}return[0,a]}function
pz(a){var
b=a[2],d=c(aq,f[3],a[1]);return[0,c(aq,f[3],b),d]}function
pA(b,a){var
e=a[2],g=a[1],h=b[2],i=b[1];function
d(d,b,a){if(b){var
e=b[1];return a?[0,c(d,e,a[1])]:b}return a?a:0}var
j=d(f[39],h,e);return h5([0,d(f[38],i,g),j])}function
fM(b){var
d=b[1];if(d){var
e=b[2];if(e){var
g=e[1],h=a(f[24],d[1]),i=a(f[22],g),j=c(f[4],i,h);return[0,c(f[1],j,pB)]}}return 0}function
pC(e,d){var
a=fM(e),b=fM(d);return a?b?c(f[29],a[1],b[1]):1:0}var
b5=[0,h5,pz,pA,fM,pC,function(d,a){var
b=d[2],e=d[1];if(e){var
g=e[1];if(b){var
i=b[1],h=c(f[29],g,a);return h?c(f[29],a,i):h}return c(f[29],g,a)}return b?c(f[29],a,b[1]):1}],aL=a(ec[1],[0,aa]),h6=p[7],ad=a(bn[19],[0,h6[1],h6[2]]),h7=[0,h[7]],b6=[a0,pE,aZ(0)],pD=0;function
pF(a){function
d(h,g){var
a=h,b=g;for(;;){switch(a[0]){case
0:return c(aL[4],a[1],b);case
1:var
f=a[3],e=a[2];break;default:var
f=a[2],e=a[1]}var
a=e,b=d(f,b);continue}}return d(a,aL[1])}function
cP(b,a){switch(a[0]){case
0:return e(n[1],b,pG,a[1]);case
1:return d5(n[1],b,pH,a[1],cP,a[2],cP,a[3]);default:return C(n[1],b,pI,cP,a[1],cP,a[2])}}function
fN(d,b){if(b){var
e=a(f[40],b[1]);return c(h[54],d,e)}return c(h[54],d,pJ)}function
h8(b,a){return C(n[1],b,pK,fN,a[1],fN,a[2])}function
pL(a,b){c(h[54],a,pM);var
d=0;function
f(b,c){return e(n[1],a,pN,b)}e(aL[15],f,b,d);return c(h[54],a,pO)}function
pP(a,b){c(h[54],a,pQ);var
d=0;function
f(b,c){return e(n[1],a,pR,b)}e(aL[15],f,b,d);return c(h[54],a,pS)}function
pT(b,a){return h8(b,a[1])}function
h9(b,d){var
g=d[2],i=g[2],j=g[1],k=d[1];if(j){var
l=a(f[40],j[1]);e(n[1],b,pU,l)}c(p[7][4],b,k);if(i){var
m=a(f[40],i[1]);return e(n[1],b,pV,m)}return c(h[54],b,pW)}function
pX(b,a){function
d(c,a){return h9(b,[0,c,a[1][1]])}return c(ad[12],d,a)}function
pY(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=p[7][4],i=a(f[40],e);return C(n[1],c,pZ,i,h,d,g)}function
h_(b,a){var
d=b[4],e=b[3],g=a[4],h=a[2],i=a[1],j=b[2],k=b[1];if(e===a[3])if(d===g){var
f=c(b5[3],k,i);return f?[0,[0,f[1],[2,j,h],e,d]]:0}throw[0,D,p0]}function
p1(f,b,d){try{var
a=c(ad[7],d,f),g=h_(b,a[1]);if(g){a[1]=g[1];var
h=0;return h}throw[0,b6,[2,b[2],a[1][2]]]}catch(a){a=w(a);if(a===O)return e(ad[10],d,f,[0,b]);throw a}}var
h$=[a0,p2,aZ(0)];function
ey(d,c,b){var
e=h7[1];if(a(ad[15],b)<e)return p1(d,c,b);throw h$}function
ez(d,b){var
k=a(b5[1],b[1]);if(k){var
l=k[1],i=l[2],j=l[1];if(d){var
e=d[1][2],h=function(a){return c(f[9],a,e)};if(1===a(f[25],e))var
o=b[4],p=b[3],q=b[2],r=c(aq,h,i),m=[0,[0,c(aq,h,j),r],q,p,o];else
var
t=b[3],u=b[4],v=b[2],w=c(aq,h,j),m=[0,[0,c(aq,h,i),w],v,u,t];if(c(f[31],e,p3))var
s=function(a){var
b=a[1];return[0,b,c(f[9],a[2],e)]},n=c(g[17],s,d);else
var
n=d;return[0,n,m]}return c(b5[6],[0,j,i],p4)?1:0}return 0}function
p5(a){return 0===a?f[26]:f[30]}function
fO(h){var
d=0,c=0,b=h;for(;;){if(b){var
e=b[2],g=a(f[25],b[1][2]);if(0===g)throw[0,D,p6];if(1===g){var
c=c+1|0,b=e;continue}var
d=d+1|0,b=e;continue}return[0,d,c]}}function
fP(a,e){var
b=a[3],c=a[1],f=a[2],d=fO(c),g=d[2],h=d[1],i=[0,e],j=0===f?[0,[0,b],[0,b]]:[0,[0,b],0];return ez(c,[0,j,i,g,h])}function
ia(d){var
b=a(ad[1],1e3);function
f(b,a){return[0,b,a]}var
h=c(l[23],f,d),i=aL[1];function
j(f,d){var
h=d[2],i=d[1],a=fP(i,h);if(typeof
a==="number"){if(0===a)throw[0,b6,[0,h]];return f}ey(a[1],a[2],b);var
j=i[1];function
k(b,a){return c(aL[4],a[1],b)}return e(g[20],k,f,j)}return[0,b,e(g[20],j,i,h)]}function
fQ(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return e(ad[14],d,b,c)}function
eA(e,b){var
h=b[2],i=e[2],j=b[1],k=e[1];if(c(f[31],i,p7))if(c(f[31],h,p8)){var
d=function(s,r){var
b=s,a=r;for(;;){if(b){if(a){var
j=a[2],m=a[1],n=m[2],k=m[1],l=b[2],o=b[1],p=o[2],e=o[1];if(e===k){var
t=c(f[9],n,h),u=c(f[9],p,i),q=c(f[1],u,t);if(c(f[26],q,p9)){var
b=l,a=j;continue}return[0,[0,e,q],d(l,j)]}if(e<k){var
v=d(l,a);return[0,[0,e,c(f[9],p,i)],v]}var
w=d(b,j);return[0,[0,k,c(f[9],n,h)],w]}var
x=function(a){var
b=a[1];return[0,b,c(f[9],a[2],i)]};return c(g[17],x,b)}if(a){var
y=function(a){var
b=a[1];return[0,b,c(f[9],a[2],h)]};return c(g[17],y,a)}return 0}},a=d(k,j);return[0,a,fO(a)]}throw[0,D,p_]}function
ib(q,g,b,e){var
h=e[3],i=e[2],j=e[1],k=c(p[7][17],q,g);if(k){var
l=k[1],d=function(c,a){return a?[0,[0,l,g,[0,[0,[0,a[1]],0],b[2],b[3],b[4]]],c]:c},m=b[1],n=m[2],o=m[1];if(1===a(f[25],l)){var
r=d(h,n);return[0,d(j,o),i,r]}var
s=d(h,o);return[0,d(j,n),i,s]}return[0,j,[0,[0,g,b],i],h]}function
ic(d,b){var
j=b[1];function
k(c,b,a){return ib(d,c,b[1],a)}var
h=e(ad[14],k,j,p$),l=h[3],m=h[2],n=h[1],o=a(ad[15],b[1]),i=a(ad[1],o);function
p(a){return e(ad[10],i,a[1],[0,a[2]])}c(g[15],p,m);function
q(e){function
b(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=a(ex,k[1][1]),t=a(ex,r[1]),u=a(f[3],j),v=c(f[9],t,u),w=c(f[9],s,l),x=c(f[1],w,v),m=eA([0,q,l],[0,p,a(f[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,d,k[2],h[2]],n[2],n[1]],b=ez(m[1],o);if(typeof
b==="number"){if(0===b)throw[0,b6,o[2]];return 0}return ey(b[1],b[2],i)}return c(g[15],b,l)}c(g[15],q,n);return[0,i,c(aL[6],d,b[2])]}function
id(h,g,t,s,r,e){var
b=e[2],d=e[1],i=c(p[7][17],h,d);if(i){var
j=i[1],k=c(f[30],j,qa)?a(f[3],g):g,l=a(f[15],j),m=eA([0,t,k],[0,d,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=c(f[9],s,k),o=function(a){var
b=c(f[9],a,l);return c(f[1],x,b)},q=b[1],y=q[1],z=c(aq,o,q[2]),A=[0,c(aq,o,y),z];return[0,w,[0,A,[1,h,r,b[2]],u,v]]}return[0,d,b]}function
ie(d,e,h,g,b){var
i=a(ex,c(p[7][17],d,e)),j=a(ad[15],b[1]),f=a(ad[1],j),k=b[1];function
l(k,j){var
c=id(d,i,e,h,g,[0,k,j[1]]),b=c[2],a=ez(c[1],b);if(typeof
a==="number"){if(0===a)throw[0,b6,b[2]];return 0}return ey(a[1],a[2],f)}c(ad[12],l,k);return[0,f,c(aL[6],d,b[2])]}function
qb(a){var
b=0;function
c(c,a,b){return(b+a[1][4]|0)+a[1][3]|0}return e(ad[14],c,a,b)}var
bM=a(bJ[1],[0,aa]);function
qc(c,b){var
d=0;function
g(d,b,g){var
e=a(f[40],b);return G(n[1],c,qd,d,e)}return e(bM[13],g,b,d)}function
ig(k,a){function
d(m,e,l){var
b=m,a=l;for(;;){if(b){var
g=b[2],h=b[1],i=h[2],j=h[1];try{var
n=c(bM[27],j,k),o=c(f[6],n,i),p=d(g,c(f[1],e,o),a);return p}catch(c){c=w(c);if(c===O){var
b=g,a=[0,[0,j,i],a];continue}throw c}}return[0,e,a]}}return d(a,qe,0)}function
ih(g,e,d){function
b(a){var
b=c(f[4],a,e);return c(f[9],b,g)}var
i=d[2],j=d[1],k=a(f[25],g);if(0===k)return c(b5[6],d,e)?qf:a(h[2],qg);if(1===k){var
l=c(aq,b,i);return[0,c(aq,b,j),l]}var
m=c(aq,b,j);return[0,c(aq,b,i),m]}function
fR(g,f,b){function
d(k,j,i){var
b=ig(g,k),l=b[1],d=c(p[7][17],f,b[2]),m=d?d[1]:qj,n=ih(m,l,j[1][1]),e=c(b5[3],i,n);return e?e[1]:a(h[2],qi)}return e(ad[14],d,b,qh)}function
ii(d){var
e=d[1];if(e){var
g=d[2],b=e[1];if(g){var
h=g[1];if(c(f[29],b,qk))if(c(f[29],ql,h))return qm;var
k=a(f[22],h),l=a(f[24],b);return c(f[29],l,k)?a(f[24],b):b}return c(f[29],b,qn)?qo:a(f[24],b)}var
i=d[2];if(i){var
j=i[1],m=a(f[22],j);return c(f[29],qp,m)?qq:a(f[22],j)}return qr}function
ij(h,l,k,d,b){function
e(b,f){var
m=a(l,b);try{var
q=function(a){return a[1][1]!==h?1:0},d=c(g[33],q,m)[1],j=d[1],r=e(ie(j,d[2],d[3],d[4],b),[0,[0,j,b],f]);return r}catch(d){d=w(d);if(d===O){var
n=a(k,b);try{var
o=function(a){return a[1]!==h?1:0},i=c(g[33],o,n)[1],p=e(ic(i,b),[0,[0,i,b],f]);return p}catch(a){a=w(a);if(a===O)return[0,[0,b,f]];throw a}}throw d}}return e(d,b)}function
fS(d,c,b,a){try{var
e=ij(d,c,b,ia(a),0);return e}catch(a){a=w(a);if(a[1]===b6)return[1,a[2]];throw a}}function
ik(w,v){var
d=v,c=0,i=0,j=0,h=0;for(;;){if(d){var
k=d[2],n=d[1],b=n[2],l=n[1];if(l){var
o=l[2],p=l[1],x=p[2];if(w===p[1]){var
m=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(b),q=b[1],r=q[2],s=q[1];if(1===a(f[25],x)){var
y=m(h,r),d=k,c=[0,[0,o,b],c],i=m(i,s),h=y;continue}var
z=m(h,s),d=k,c=[0,[0,o,b],c],i=m(i,r),h=z;continue}var
d=k,c=[0,[0,l,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
d=k,c=[0,[0,0,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
t=a(g[1],i),A=0,B=function(b,a){return b+a|0},C=e(g[20],B,A,i),u=a(g[1],h),D=0,E=function(b,a){return b+a|0};return[0,c,j+u*C+t*e(g[20],E,D,h)-u*t]}}var
fT=[0,ik,function(a){var
b=a[2],d=[0,0,fQ(a)];function
f(b,a){var
d=a[1],c=ik(b,a[2]);return[0,[0,[0,b,c[2]],d],c[1]]}var
h=e(aL[15],f,b,d)[1];function
i(b,a){return j6(b[2],a[2])}return c(g[48],i,h)}];function
fU(a){var
b=a[1];if(b){var
d=a[2];if(d)return c(f[26],b[1],d[1])}return 0}function
qs(a,h){var
b=a[1];if(b){var
d=a[2];if(d){var
e=d[1],g=c(f[26],b[1],e);return g?c(f[26],h,e):g}}return 0}function
fV(b,e){var
a=e;for(;;){if(a){var
c=a[2],d=a[1][1];if(d===b)return[0,1,c];if(d<b){var
a=c;continue}return[0,0,a]}return qt}}function
il(d){var
a=d;for(;;){if(a){var
b=a[1],c=b[1],e=a[2],f=b[4],g=b[3],h=b[2];if(c)if(!c[2])return[0,[0,c[1][1],c,h,g,f]];var
a=e;continue}return 0}}function
im(f,k){var
j=il(f);if(j)return[0,j[1]];var
b=f;a:for(;;){if(b){var
c=b[1],i=c[1],a=i,o=b[2],p=c[4],q=c[3],r=c[2];for(;;){if(a){var
h=a[1][1],n=a[2],l=0,m=function(d){return function(a,b){var
c=b[2];return fV(d,b[1])[1]?fU(c[1])?a+1|0:a:a}}(h);if(2!==e(g[20],m,l,k)){var
a=n;continue}var
d=[0,h]}else
var
d=0;if(d)return[0,[0,d[1],i,r,q,p]];var
b=o;continue a}}return 0}}var
fW=[0,fU,qs,fV,il,im,function(i){var
h=fQ(i),j=0;function
k(b,d){var
a=d[2],e=a[1],g=e[1],j=d[1];if(g){var
h=e[2];if(h){var
i=g[1];return c(f[26],i,h[1])?[0,[0,j,i,a[2],a[4]+a[3]|0],b]:b}}return b}var
b=e(g[20],k,j,h),d=im(b,h);if(d){var
a=d[1];return[0,[0,[0,a[1],a[2],a[3],a[4]],0],0]}var
l=0;function
m(s,f){var
p=f[1],e=p,n=h,k=s,t=f[4],u=f[3],v=f[2];a:for(;;){if(e){var
o=e[1][1],c=n,b=0,a=0,q=e[2],r=t-1|0;for(;;){if(c){var
g=c[2],l=c[1],d=l[2],i=d[3]+d[4]|0,m=fV(o,l[1]),j=m[2];if(0===m[1]){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}if(fU(d[1])){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}var
c=g,b=(b+i|0)+r|0,a=[0,[0,j,d],a];continue}var
e=q,n=a,k=[0,[0,[0,o,p,v,u],b],k];continue a}}return k}}var
n=e(g[20],m,l,b);function
o(b,a){return aa(b[2],a[2])}return c(g[48],o,n)}];function
qu(i,d){var
j=0;function
k(d,b){var
e=a(p[7][18],b[1]);return c(h[5],d,e)}var
b=e(g[20],k,j,d),l=[0,[0,e(p[7][11],b,qw,i),0,qv],d],f=fS(b,fW[6],fT[2],l);if(0===f[0]){var
m=f[1][1];try{var
q=[0,fR(bM[1],b,m[1])];return q}catch(b){b=w(b);if(a(br[20],b)){var
o=a(ed[1],b);c(n[2],qx,o);return 0}throw b}}return 0}var
qy=[0,qu,function(j){var
d=fS(h[7],fW[6],fT[2],j);if(0===d[0]){var
c=d[1][2],b=bM[1];for(;;){if(c){var
f=c[1],i=f[1],k=c[2],l=ii(fR(b,i,f[2][1])),c=k,b=e(bM[4],i,l,b);continue}var
m=0,n=function(c,b,a){return[0,[0,c,b],a]},o=e(bM[13],n,b,m);return[0,a(g[9],o)]}}return[1,d[1]]}];function
bs(b,a){return eA(b,a)[1]}function
io(d,b,a){var
f=0;function
h(b,f){function
h(a,e){var
b=c(d,f,e);return b?[0,b[1],a]:a}return e(g[20],h,b,a)}return e(g[20],h,f,b)}function
eB(b,a){if(0===b)if(0===a)return 0;return 1}function
fX(s,r,q){var
j=q[2],k=q[1],l=r[2],m=r[1],n=j[3],e=j[2],g=j[1],o=l[3],h=l[2],i=l[1],t=c(p[7][17],s,i),u=c(p[7][17],s,g);if(t)if(u){var
b=u[1],d=t[1],v=a(f[25],b);if(-1===e1(a(f[25],d),v)){var
w=a(f[15],b),x=c(f[9],n,w),y=a(f[15],d),z=c(f[9],o,y),A=c(f[1],z,x),B=eB(h,e),C=[0,g,a(f[15],b)],D=[0,bs([0,i,a(f[15],d)],C),B,A],E=[0,k,a(f[15],b)];return[0,[0,bs([0,m,a(f[15],d)],E),D]]}if(0===h){var
F=c(f[9],n,qz),G=c(f[9],d,b),H=a(f[3],G),I=c(f[9],o,H),J=c(f[1],I,F),K=eB(h,e),L=c(f[9],d,b),M=[0,bs([0,i,a(f[3],L)],[0,g,qA]),K,J],N=c(f[9],d,b);return[0,[0,bs([0,m,a(f[3],N)],[0,k,qB]),M]]}if(0===e){var
O=c(f[9],o,qC),P=c(f[9],b,d),Q=a(f[3],P),R=c(f[9],n,Q),S=c(f[1],R,O),T=eB(h,e),U=c(f[9],b,d),V=[0,bs([0,g,a(f[3],U)],[0,i,qD]),T,S],W=c(f[9],b,d);return[0,[0,bs([0,k,a(f[3],W)],[0,m,qE]),V]]}return 0}return 0}function
ip(a){function
b(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=fP(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b}return e(g[20],b,qF,a)}function
iq(t,e,s){var
k=e[2],l=e[1],m=e[4][1],u=m[2],v=m[1];function
n(e,b,a){if(b){var
f=b[1][3];if(a){var
d=a[1];return c(e,f,d)?[0,[0,l,k,d]]:b}return b}return a?[0,[0,l,k,a[1]]]:0}var
b=n(f[29],t,v),d=n(f[30],s,u);if(b)if(d){var
g=d[1],i=g[2],o=g[1],j=b[1],p=j[1],w=j[2];if(c(f[29],j[3],g[3]))return[0,[0,b,d]];var
q=i[1];if(q){var
r=fX(q[1][1],[0,p,w],[0,o,i]);return r?[1,r[1]]:a(h[2],qG)}return[1,[0,bs([0,p,qI],[0,o,qH]),i]]}return[0,[0,b,d]]}var
B=[0,pw,aq,ex,px,py,b5,aL,pD,ad,h7,b6,pF,cP,fN,h8,pL,pP,pT,h9,pX,pY,h_,h$,ey,ez,p5,fO,fP,ia,fQ,eA,ib,ic,id,ie,qb,bM,qc,ig,ih,fR,ii,ij,fS,fT,fW,qy,[0,bs,io,eB,fX,ip,iq,function(t,a){function
b(a){switch(a[0]){case
0:var
j=a[1];return[0,[0,[0,[0,j,qJ],0],c(g[7],t,j)],0];case
1:var
u=a[3],v=a[1],w=b(a[2]),x=b(u);return io(function(a,b){return fX(v,a,b)},w,x);default:var
y=a[2],z=b(a[1]),A=b(y),f=ip(c(h[25],z,A));if(0===f[0]){var
B=f[1],C=function(a,c){if(0===a[0]){var
b=a[1];return iq(b[1],c,b[2])}return a},i=e(g[20],C,qK,B);if(0===i[0]){var
k=i[1],d=k[2],l=k[1];if(l){var
m=l[1],n=m[2],o=m[1];if(d){var
p=d[1];return[0,[0,o,n],[0,[0,p[1],p[2]],0]]}var
r=n,q=o}else{if(!d)return 0;var
s=d[1],r=s[2],q=s[1]}return[0,[0,q,r],0]}return[0,i[1],0]}return[0,f[1],0]}}return b(a)}]];aY(744,B,"Micromega_plugin.Mfourier");var
qL=0,qM=0,qN=0,qO=s[13],qP=s[12][8],qR=0;function
qS(b){return[1,a(l[29][7],b)]}var
cQ=[0,l[30][7],qS,qR,qQ,qP,qO],qT=s[77],qU=s[80],qX=l[29][9],bt=[0,function(b){return[0,a(l[30][7],b),0]},qX,qW,qV,qU,qT];function
cR(e,b){function
d(b){switch(b[0]){case
0:var
g=a(e[2],b[1]);return a(p[6][4],g);case
1:var
h=a(l[29][2],b[1]);return a(p[6][2],h);case
2:var
i=b[1],j=d(b[2]),k=d(i);return c(p[6][7],k,j);case
3:var
m=b[1],n=d(b[2]),o=a(p[6][8],n),q=d(m);return c(p[6][7],q,o);case
4:var
r=b[2],s=d(b[1]),t=d(r);return c(p[6][6],s,t);case
5:var
u=d(b[1]);return a(p[6][8],u);default:var
v=b[2],w=d(b[1]),x=a(l[29][3],v),f=function(b){if(0===b){var
d=a(e[2],e[4]);return a(p[6][4],d)}var
g=f(b-1|0);return c(p[6][6],w,g)};return f(x)}}return d(b)}function
ir(b){function
c(f,c,b){var
d=a(l[30][2],f),e=1===c?[1,d]:[6,[1,d],a(l[30][3],c)];return X(b,qZ)?e:[4,e,b]}return e(p[5][12],c,b,qY)}function
is(o,h){function
p(b){var
d=a(f[24],b);return c(f[26],d,b)}if(c(g[27],p,h)){var
i=function(a){return ir(c(g[7],o,a))},e=q3,b=0,d=h;for(;;){if(d){var
j=d[2],k=d[1];if(c(f[26],k,q0)){var
b=b+1|0,d=j;continue}var
q=a(l[18],k),m=[0,a(l[30][7],q)],n=X(m,q1)?i(b):[4,m,i(b)],r=X(e,q2)?n:[2,n,e],e=r,b=b+1|0,d=j;continue}return e}}throw[0,D,q4]}function
it(e,d){var
b=d;for(;;){var
c=a(e,b);if(X(c,b))return c;var
b=c;continue}}function
iu(b,e){var
d=G(s[74],b[3],b[4],b[5],b[6]);function
c(b){if(typeof
b!=="number")switch(b[0]){case
3:var
e=b[1],f=c(b[2]);return a(d,[3,c(e),f]);case
4:var
g=b[1],h=c(b[2]);return a(d,[4,c(g),h])}return a(d,b)}return c(e)}function
eC(b,a){return it(function(a){return iu(b,a)},a)}function
iv(d){function
j(p,o){var
f=p,d=o;for(;;){if(f){var
g=f[2],a=f[1];if(d){var
c=d[1];if(typeof
c==="number")var
b=0;else
switch(c[0]){case
2:if(typeof
a==="number")var
b=0;else
if(2===a[0]){var
h=c[1],k=a[2],l=c[2];if(X(h,a[1]))var
e=[0,[2,h,[4,l,k]]],b=1;else
var
e=0,b=1}else
var
b=0;break;case
3:if(typeof
a==="number")var
b=0;else
if(3===a[0]){var
i=c[1],m=a[2],n=c[2];if(X(i,a[1]))var
e=[0,[3,i,[4,n,m]]],b=1;else
var
e=0,b=1}else
var
b=0;break;default:var
b=0}if(!b)var
e=0;if(e){var
f=g,d=[0,e[1]];continue}return[4,c,j(g,[0,a])]}var
f=g,d=[0,a];continue}return d?d[1]:0}}var
a=d,b=0,f=0;for(;;){if(typeof
a!=="number"&&4===a[0]){var
e=[0,a[1],b],a=a[2],b=e;continue}return j(c(g[48],I.caml_compare,[0,a,b]),f)}}function
iw(b,f){var
h=0;function
i(d,a){return[0,c(p[6][1],b,a),d]}var
d=e(g[20],i,h,f);if(X(b,p[5][1])){var
j=[1,q[1]],k=a(g[9],d);return[0,a(p[7][5],[0,[1,q[2]],k]),0,j]}var
l=[1,q[1]],m=a(g[9],d);return[0,a(p[7][5],[0,[1,q[1]],m]),0,l]}function
ix(a){function
c(f,d){var
b=f,a=d;for(;;){if(a){if(0===a[1][2]){var
b=b+1|0,a=a[2];continue}var
g=c(b+1|0,a[2]),h=1,i=p[7][3],j=function(a){return q6};return[0,[0,e(p[7][10],b+1|0,j,i),h,q5],g]}return 0}}return c(0,a)}function
q7(a){switch(a){case
0:return q8;case
1:return q9;case
2:return q_;default:return q$}}var
eD=a(ec[1],[0,p[5][10]]);function
iy(b){function
f(a){return a[1]}var
d=c(g[17],f,b),i=a(eD[5],p[5][1]);function
j(b,a){function
d(b,d,a){return c(eD[4],b,a)}return e(p[6][9],d,a,b)}var
k=e(g[20],j,i,d),l=0;function
m(b,a){return[0,iw(b,d),a]}var
n=e(eD[15],m,k,l),o=[1,q[2]],r=1;function
s(a){return 2===a[2]?[1,q[2]]:[1,q[1]]}var
t=c(g[17],s,b),u=[0,a(p[7][5],[0,[1,q[2]],t]),r,o],v=[0,u,ix(b)],w=c(h[25],v,n),x=[1,q[1]];return[0,[0,a(p[7][5],[0,[1,q[2]],0]),1,x],w]}var
ra=l[30][7];function
fY(d,b){var
e=b[1],g=d[1],f=b[2];if(e){var
j=e[2],i=function(d,b){if(d){var
e=d[1],m=d[2];if(b){var
j=b[1],f=i(m,b[2]),k=c(q[23],e,q[1]);if(-1===k){var
n=[0,a(l[30][1],j)];return[4,[2,[0,a(g,e)],n],f]}if(0===k)return f;var
o=[0,a(l[30][1],j)];return[4,[3,[5,a(g,e)],o],f]}return a(h[2],rb)}return 0};return iv(eC(d,i(j,f)))}return a(h[2],rc)}var
re=[a0,rd,aZ(0)],fZ=[a0,rf,aZ(0)],aM=a(bJ[1],[0,p[5][10]]);function
iz(b){var
d=[0,0];function
o(b,a){return aa(b[1],a[1])}var
h=[0,aM[1],0];function
i(i,h){var
j=i[2],k=i[1],q=h[2],m=[0,h[1],0];function
n(g,j,i){var
h=i[2],b=i[1];if(X(g,p[5][1]))return[0,b,h];try{var
q=c(aM[27],g,b),l=q,k=b}catch(a){a=w(a);if(a!==O)throw a;var
m=e(aM[4],g,d[1],b),n=d[1];d[1]++;var
l=n,k=m}var
o=0===a(f[25],j)?h:[0,[0,l,j],h];return[0,k,o]}var
b=e(p[6][9],n,k,m),r=b[2],s=b[1],t=c(p[6][1],p[5][1],k),u=a(f[3],t);if(0===j)var
l=0;else{if(!(3<=j))throw fZ;var
l=1}return[0,s,[0,[0,c(g[48],o,r),l,u],q]]}return e(g[21],i,b,h)[2]}function
iA(e){var
f=iy(e);try{var
b=a(B[47][2],f);if(0===b[0])var
i=a(p[7][8],b[1]),d=[0,a(l[22],i)];else
var
d=0;return d}catch(b){b=w(b);if(a(br[20],b)){if(B[4]){var
g=a(ed[1],b);c(n[2],rg,g);a(h[51],h[27])}return 0}throw b}}function
iB(b){try{var
d=iz(b),f=a(B[47][2],d);if(0===f[0])var
h=0;else{var
i=f[1];if(B[4])e(n[2],rh,B[13],i);var
k=c(B[48][7],d,i),m=a(g[5],k)[1],o=function(a){return[0,a[1]+1|0,a[2]]},j=c(g[17],o,m);if(B[4])e(n[2],ri,p[7][4],j);var
q=a(p[7][8],j),h=[0,a(l[22],q)]}return h}catch(a){a=w(a);if(a===fZ)return iA(b);throw a}}function
iC(d){var
b=a(g[46],d),e=b[2],c=iB(b[1]);return c?[0,[0,c[1],e]]:0}function
rj(e,b){var
d=a(g[1],b)-1|0,f=c(l[14],0,d),i=c(g[47],b,f);function
j(a){return 1===a[1][2]?1:0}var
k=c(g[37],j,i)[2];function
m(b){var
c=b[1],d=c[2],f=b[2],g=c[1];return 1===d?a(h[2],rk):[0,[0,cR(e,g),d],f]}return iC(c(g[17],m,k))}function
iD(c,b){try{var
e=rj(c,b);return e}catch(b){b=w(b);if(a(br[20],b)){var
d=a(ed[1],b);a(h[30],d);return 0}throw b}}function
cS(e,b){var
d=a(g[1],e),f=c(h[5],b,e1(d,b));return c(h[5],d,f)}function
rl(d,b,a){var
e=cS(a,d);B[10][1]=e;var
c=iD(b,a);return c?[0,fY(b,c[1])]:0}function
rm(n,b){a(p[22][1][6],0);var
o=cS(b,n);B[10][1]=o;function
q(c,b){return[0,c,[0,a(l[30][1],b)]]}var
f=c(l[23],q,b);function
r(b,a){var
d=a[1],e=b[1],f=d[1],g=e[1],h=[3,b[2],a[2]];return[0,[0,[4,g,f],c(s[53],e[2],d[2])],h]}var
t=c(l[9],r,f),u=0;function
v(b,a){var
c=a[1],d=c[2],e=a[2],f=c[1];return d?[0,[0,[0,f,d[1]],e],b]:b}var
w=e(g[20],v,u,t),i=c(h[25],f,w);function
x(a){return cR(bt,a[1][1])}var
y=c(g[17],x,i),z=aM[1];function
A(c,b){function
d(c,f,b){var
d=a(p[5][13],c);return d?e(aM[4],d[1],c,b):b}return e(p[6][9],d,b,c)}var
C=e(g[20],A,z,y);function
j(b){var
c=[0,bt[4]];function
d(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}return e(p[5][12],d,b,c)}var
D=d5(s[63],bt[3],bt[4],s[79],s[80],s[82],s[81],s[77]),E=0;function
F(d,c,b){var
e=[1,a(D,j(d))];return[0,[0,[0,j(c),3],e],b]}var
G=e(aM[13],F,C,E),k=c(h[25],i,G);function
H(a){return a[1]}var
m=iD(bt,c(g[17],H,k));if(m){var
I=fY(bt,m[1]),d=function(b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:var
e=a(l[29][1],b[1]);return c(g[7],k,e)[2];case
1:return[1,b[1]];case
2:var
f=b[1];return[2,f,d(b[2])];case
3:var
h=b[1],i=d(b[2]);return[3,d(h),i];case
4:var
j=b[1],m=d(b[2]);return[4,d(j),m];default:return[5,b[1]]}};return[0,d(I)]}return 0}function
rn(b){function
h(a){return a[1]}var
i=c(g[17],h,b),j=a(p[6][4],ro);function
k(b,a){return c(p[6][7],a,b)}var
l=e(g[20],k,j,i),m=0;function
n(b,c,a){return X(b,p[5][1])?a:[0,b,a]}var
d=e(p[6][9],n,l,m);function
o(b){var
e=b[1],h=b[2],i=c(p[6][1],p[5][1],e),j=a(f[3],i);function
k(a){return c(p[6][1],a,e)}var
l=c(g[17],k,d);return[0,a(p[7][5],l),h,j]}return[0,c(g[17],o,b),d]}function
rp(b,a){return[2,b,a]}function
rq(b,a){return[4,b,a]}function
rr(a){return[0,a]}function
rs(a){return[5,a]}function
rt(e,d,b){var
a=b;for(;;){if(a){var
f=a[2];if(c(e,d,a[1]))return 1;var
a=f;continue}return 0}}function
iE(d,b,g){var
a=g;for(;;){if(a){var
e=a[2],f=a[1];if(c(d,b,f[1])){var
a=e;continue}return[0,f,iE(d,b,e)]}return 0}}function
iF(b,a){return 0===c(p[7][15],b,a)?1:0}function
ru(c,a){var
b=0;function
d(b,a){return iF(a,c)?b:[0,a,b]}return e(g[20],d,b,a)}function
f0(b,a){var
c=a[2],d=a[1];if(0===c)return[0,cR(b,d),0];if(3<=c)return[0,cR(b,d),1];throw[0,D,rv]}function
iG(a){return 0===a?0:3}function
rw(d){var
b=a(g[46],d),e=b[1],f=a(l[22],b[2]);function
h(a){return[1,a]}var
i=c(g[17],h,f);return c(g[47],e,i)}function
iH(b){var
f=b[3],h=b[2],e=a(g[46],b[1]),i=e[1],d=a(l[22],[0,f,e[2]]);if(d){var
j=d[2],k=[1,d[1]],m=function(a){return[1,a]},n=c(g[17],m,j);return[0,c(g[47],i,n),h,k]}throw[0,D,rx]}function
ry(e,d){var
b=iH(d),c=b[3],f=b[2],g=b[1];try{var
i=is(e,a(p[7][8],g)),j=a(l[17],c),k=a(l[30][7],j),m=a(l[18],c),n=a(l[30][7],m),o=[0,[2,[4,[0,k],i],[5,[0,n]]],iG(f)];return o}catch(b){b=w(b);if(b[1]===fe)return a(h[2],rz);throw b}}function
bN(b){if(typeof
b==="number")return[0,q[2],0];else
switch(b[0]){case
0:var
e=b[1],z=[0,[1,a(l[18],e)]];return[0,a(l[17],e),z];case
1:return[0,q[2],[1,b[1]]];case
2:return a(h[2],rA);case
3:var
f=bN(b[1]);return[0,f[1],[3,f[2]]];case
4:var
g=b[1],A=g[2],i=bN(g[1]),j=i[2],k=i[1],m=bN(A),n=m[2],o=m[1],d=c(q[17],k,o),p=c(q[15],k,d),r=c(q[15],o,d),B=c(q[10],p,r),s=c(q[10],d,B);return 0===c(q[23],s,q[2])?[0,q[2],[4,[0,j,n]]]:[0,s,[4,[0,[6,[0,[0,[1,r]],j]],[6,[0,[0,[1,p]],n]]]]];case
5:return a(h[2],rB);case
6:var
t=b[1],C=t[2],u=bN(t[1]),D=u[2],E=u[1],v=bN(C),F=[6,[0,D,v[2]]];return[0,c(q[10],E,v[1]),F];case
7:return a(h[2],rC);default:var
w=b[1],x=w[2],y=bN(w[1]),G=[8,[0,y[2],x]];return[0,c(q[19],y[1],x),G]}}function
f1(b){var
a=bN(b);return[0,a[1],a[2]]}function
rD(i,g,f){var
d=0,c=0,b=f;for(;;){if(b){var
e=b[2];if(a(i,b[1])){if(d===g)return c;var
d=d+1|0,c=c+1|0,b=e;continue}var
c=c+1|0,b=e;continue}return a(h[2],rE)}}function
bO(b){switch(b[0]){case
0:return[0,q[2],[0,b[1]]];case
1:return[0,q[2],[1,b[1]]];case
2:return[0,q[2],[2,b[1]]];case
3:var
e=b[1],v=[3,[1,a(l[18],e)]];return[0,a(l[17],e),v];case
4:var
f=b[1],w=[4,[1,a(l[18],f)]];return[0,a(l[17],f),w];case
5:var
g=b[1],x=[5,[1,a(l[18],g)]];return[0,a(l[17],g),x];case
6:var
h=f1(b[1]),i=h[1],y=[6,h[2]];return[0,c(q[10],i,i),y];case
7:return[0,q[2],[7,b[1]]];case
8:var
z=b[2],j=f1(b[1]),A=j[2],B=j[1],k=bO(z),C=[8,A,k[2]];return[0,c(q[10],B,k[1]),C];case
9:var
D=b[2],m=bO(b[1]),n=m[1],E=m[2],o=bO(D),p=o[1],F=o[2],d=c(q[17],n,p),r=c(q[15],n,d),s=c(q[15],p,d),G=c(q[10],r,s);return[0,c(q[10],d,G),[9,[10,[4,[1,s]],E],[10,[4,[1,r]],F]]];default:var
H=b[2],t=bO(b[1]),I=t[2],J=t[1],u=bO(H),K=[10,I,u[2]];return[0,c(q[10],J,u[1]),K]}}function
aN(b){if(typeof
b==="number")return[0,a(l[30][8],rF)];else
switch(b[0]){case
0:return[0,a(l[30][8],b[1])];case
1:var
c=b[1],j=j8(e(iI[4],c,1,j7(c)-1|0));return[1,a(l[30][4],j)];case
3:return[5,aN(b[1])];case
4:var
d=b[1],k=d[1],m=aN(d[2]);return[2,aN(k),m];case
5:var
f=b[1],n=f[1],o=aN(f[2]);return[3,aN(n),o];case
6:var
g=b[1],p=g[1],q=aN(g[2]);return[4,aN(p),q];case
8:var
i=b[1],r=i[1],s=a(l[30][3],i[2]);return[6,aN(r),s];default:return a(h[2],rG)}}function
f2(b){var
c=aN(b),d=s[77],e=s[81],f=s[82],g=s[80],h=s[79],i=a(l[30][8],rH),j=a(l[30][8],rI);return bS(s[39],j,i,h,g,f,e,d,c)}function
eE(b){if(b){var
c=b[2],d=b[1];if(c){var
e=eE(c);return[3,[0,a(l[30][1],d)],e]}return[0,a(l[30][1],d)]}return 0}function
rJ(b){function
d(b){switch(b[0]){case
0:return[0,a(l[30][1],b[1])];case
1:return[0,a(l[30][1],b[1])];case
2:return[0,a(l[30][1],b[1])];case
6:return[1,f2(b[1])];case
7:return eE(b[1]);case
8:var
g=b[1],h=d(b[2]);return[2,f2(g),h];case
9:var
i=b[1],j=d(b[2]);return[4,d(i),j];case
10:var
k=b[1],m=d(b[2]);return[3,d(k),m];default:var
e=b[1];return 0===c(f[37],e,rK)?0:[5,a(l[30][8],e)]}}return eC(bt,d(b))}function
aO(b){if(typeof
b==="number")return rL;else
switch(b[0]){case
0:var
k=a(f[52],b[1]);return[0,a(l[30][7],k)];case
1:var
c=b[1],m=j8(e(iI[4],c,1,j7(c)-1|0));return[1,a(l[30][4],m)];case
3:return[5,aO(b[1])];case
4:var
d=b[1],n=d[1],o=aO(d[2]);return[2,aO(n),o];case
5:var
g=b[1],p=g[1],q=aO(g[2]);return[3,aO(p),q];case
6:var
i=b[1],r=i[1],s=aO(i[2]);return[4,aO(r),s];case
8:var
j=b[1],t=j[1],u=a(l[30][3],j[2]);return[6,aO(t),u];default:return a(h[2],rM)}}function
f3(b){var
c=aO(b),d=s[13],e=s[12][6],f=s[12][7],g=s[12][8],h=s[12][5],i=a(l[30][5],1),j=a(l[30][5],0);return bS(s[39],j,i,h,g,f,e,d,c)}function
rN(b){var
e=bO(b)[2];function
d(k){var
b=k;for(;;)switch(b[0]){case
0:return[0,a(l[30][1],b[1])];case
1:return[0,a(l[30][1],b[1])];case
2:return[0,a(l[30][1],b[1])];case
6:return[1,f3(b[1])];case
7:return eE(b[1]);case
8:var
i=b[2],e=b[1];if(typeof
e==="number")var
g=0;else
if(0===e[0])var
j=c(f[26],e[1],rP),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
b=i;continue}var
n=d(i);return[2,f3(e),n];case
9:var
o=b[1],p=d(b[2]);return[4,d(o),p];case
10:var
q=b[1],r=d(b[2]);return[3,d(q),r];default:var
h=b[1];if(0===c(f[37],h,rO))return 0;var
m=a(f[52],h);return[5,a(l[30][7],m)]}}return eC(cQ,d(e))}var
iJ=[0,function(e,d){var
c=0,b=d;for(;;){if(b){var
f=b[2];if(X(e,b[1]))return c;var
c=c+1|0,b=f;continue}return a(h[2],rQ)}}];function
f4(b){var
c=b[1],d=a(l[18],b[2]),f=[0,a(l[30][7],d)];function
h(f,b){var
g=b[2],h=a(p[22][1][8],b[1]);function
c(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}var
d=e(p[5][12],c,h,rR),i=a(l[18],g);return[2,[4,[0,a(l[30][7],i)],d],f]}return e(g[20],h,f,c)}function
aP(d,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
2:return[5,a(l[30][7],b[1])];case
3:var
f=f4(b[1]);return[1,a(s[92],f)];case
4:var
g=b[2],i=f4(b[1]),j=a(s[92],i);return[2,j,aP(d,g)];case
6:var
k=b[1],m=aP(d,b[2]);return[3,aP(d,k),m];case
7:var
n=b[1],o=aP(d,b[2]);return[4,aP(d,n),o];case
0:case
1:var
e=c(iJ[1],b[1],d);return[0,a(l[30][1],e)];default:return a(h[2],rS)}}function
cT(b,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
e=a[3],d=a[2],f=a[1];if(typeof
d!=="number"&&8===d[0]){var
i=d[1],j=cT([0,f,b],e);return[1,aP(b,i),j]}var
h=cT([0,f,b],e);return[0,aP(b,d),h]}var
k=a[5],l=a[4],m=a[2],n=[0,a[1],b],o=function(a){return cT(n,a)},p=c(g[17],o,k),q=aP(b,l);return[2,aP(b,m),q,p]}}function
iK(e,b){var
f=1+a(p[15],b)|0,d=c(p[18],f,b)[2];if(B[4])G(n[1],h[27],rT,p[13],d);return cT(e,d)}function
eF(b){var
d=a(B[47][2],b);if(0===d[0])return 0;var
f=d[1];if(B[4])e(n[2],rU,B[13],f);var
i=c(B[48][7],b,f),h=a(g[5],i)[1];if(B[4])e(n[2],rV,p[7][4],h);var
j=a(p[7][8],h);return[0,a(l[22],j)]}function
rW(d,b){var
e=a(f[40],b);return c(h[54],d,e)}function
rX(d,b){var
e=a(q[33],b);return c(h[54],d,e)}function
eG(g,f){var
e=0,d=g,b=f;for(;;){if(b){if(d){var
i=b[2],j=d[2],k=c(p[20],b[1],d[1]),e=c(p[19],k,e),d=j,b=i;continue}return a(h[2],rY)}return e}}function
iL(d){var
b=a(g[46],d),e=b[2],c=eF(b[1]);return c?[0,eG(e,c[1])]:0}var
iM=B[4]?function(b){a(n[2],rZ);a(h[51],h[27]);var
c=iL(b);a(n[2],r0);a(h[51],h[27]);return c}:iL;function
f5(m){var
d=m[2],h=m[1],i=h[3],j=h[2],k=h[1];if(k){var
o=function(a){return a[2]},p=c(g[17],o,k),n=a(l[21],p),b=[1,n];if(c(f[32],b,r1))return[2,h,d];var
q=c(f[12],i,b);if(0===a(f[25],q)){if(1<=a(f[25],b)){var
r=c(f[9],i,b),s=function(a){var
d=a[1];return[0,d,c(f[9],a[2],b)]};return[2,[0,c(g[17],s,k),j,r],[5,n,d]]}throw[0,D,r2]}if(0===j)return[0,[8,d]];var
t=c(f[9],i,b),u=a(f[24],t),v=function(a){var
d=a[1];return[0,d,c(f[9],a[2],b)]};return[1,[0,c(g[17],v,k),j,u],[8,d]]}return e(B[26],j,r3,i)?0:[0,d]}function
iN(k,j,i){var
e=i[1],g=j[1],m=e[2],n=e[1],o=g[2],q=g[1],t=i[2],u=j[2],v=e[3],w=g[3];function
h(d,b){var
e=a(l[18],b),g=c(p[20],e,t),h=a(l[18],d),i=[7,c(p[20],h,u),g],j=c(f[6],v,b),k=c(f[6],w,d),r=c(f[1],k,j),s=c(B[48][3],o,m),x=c(p[7][13],b,n),y=c(p[7][13],d,q);return[0,[0,c(p[7][14],y,x),s,r],i]}var
r=c(p[7][17],k,q),s=c(p[7][17],k,n);if(r)if(s){var
b=s[1],d=r[1],x=a(f[25],b);if(-1===e1(a(f[25],d),x)){var
y=a(f[15],b);return[0,h(y,a(f[15],d))]}if(0===o){var
z=[0,a(f[25],d)],A=c(f[6],b,z),C=a(f[3],A);return[0,h(C,a(f[15],d))]}if(0===m){var
D=a(f[15],b),E=[0,a(f[25],b)],F=c(f[6],d,E);return[0,h(D,a(f[3],F))]}return 0}return 0}var
cU=[a0,r4,aZ(0)];function
iO(a){var
b=0;function
c(b,c){var
a=f5([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,cU,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return e(g[20],c,b,a)}function
cV(g,b){if(0===a(q[22],b))return[0,q[2],q[1]];var
d=c(q[14],g,b),h=d[1],e=cV(b,d[2]),f=e[2],i=e[1],j=c(q[10],h,f);return[0,f,c(q[8],i,j)]}function
r5(j,i){var
b=a(q[36],j),d=a(q[36],i),e=cV(b,d),f=e[2],g=e[1],k=c(q[10],f,d),l=c(q[10],g,b),m=c(q[5],l,k),o=a(q[33],m),p=a(q[33],d),r=a(q[33],f),s=a(q[33],b),t=a(q[33],g);return d5(n[1],h[27],r6,t,s,r,p,o)}var
r8=[a0,r7,aZ(0)];function
r9(c){function
b(a){return 0===a[1][2]?1:0}return a(g[37],b)}function
iP(r,p){var
f=p[1],g=r[1];if(0===g[2])if(0===f[2]){var
d=g[1],b=f[1];for(;;){if(d)if(b){var
h=b[2],i=b[1],j=i[2],k=i[1],m=d[2],n=d[1],o=n[2],e=n[1];if(X(e,k)){var
s=q[2],t=a(l[18],j),u=a(l[18],o),v=c(q[17],u,t);if(0===c(q[23],v,s))return[0,[0,e,o,j]];var
d=m,b=h;continue}if(gG(e,k)){var
d=m;continue}var
b=h;continue}return 0}}return 0}function
iQ(m,k){var
d=0,b=k;for(;;){if(b){var
f=b[2],e=b[1],n=a(m,e),h=c(l[15],n,f),i=h[1];if(i){var
j=i[1],o=j[2],p=j[1];return[0,[0,[0,p,e,o]],c(g[12],d,h[2])]}var
d=[0,e,d],b=f;continue}return[0,0,d]}}function
iR(a){return iQ(iP,a)}function
eH(f,b){var
c=0;function
d(c,d){var
e=a(f,d);if(e){var
b=f5(e[1]);if(typeof
b==="number")return c;else
switch(b[0]){case
0:throw[0,cU,b[1]];case
1:return[0,[0,b[1],b[2]],c];default:return[0,[0,b[1],b[2]],c]}}return[0,d,c]}return e(g[20],d,c,b)}function
eI(c,b,a){return eH(function(a){return iN(c,b,a)},a)}function
iS(r){var
i=iR(r),j=i[1],s=i[2];if(j){var
b=j[1],k=b[3],m=k[1],n=b[2],o=n[2],d=n[1],e=b[1],t=k[2],u=e[2],v=e[1],w=a(l[18],e[3]),q=cV(a(l[18],u),w),g=[1,q[1]],h=[1,q[2]],x=c(f[6],h,m[3]),y=c(f[6],g,d[3]),z=c(f[1],y,x),A=c(p[7][13],h,m[1]),B=c(p[7][13],g,d[1]),C=[0,c(p[7][14],B,A),0,z],D=a(l[18],h),E=c(p[20],D,t),F=a(l[18],g),G=c(p[20],F,o);return[0,eI(v,[0,C,c(p[19],G,E)],[0,[0,d,o],s])]}return 0}function
iT(e){function
h(b){var
a=b[1];if(0===a[2])try{var
d=a[1],e=function(d){var
a=d[2],b=c(f[26],a,r_);return b?b:c(f[26],a,r$)},h=[0,c(g[33],e,d)[1]];return h}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
a=c(l[15],h,e),b=a[1],i=a[2];if(b){var
d=b[1];return[0,eI(d[1],d[2],i)]}return 0}function
iU(h){function
i(e){var
b=e[1];if(0===b[2])try{var
h=b[1],i=function(b){var
d=b[2],g=b[1],h=c(f[26],d,sa),e=h||c(f[26],d,sb);if(e){var
i=a(p[22][1][8],g);return a(p[5][4],i)}return e},d=c(g[33],i,h)[1],j=a(p[22][1][8],d),k=b[1],l=function(g){var
b=g[1],e=b===d?1:0;if(e)var
f=e;else
var
h=a(p[22][1][8],b),f=0===c(p[5][9],h,j)[2]?1:0;return f},m=c(g[27],l,k)?[0,d]:0;return m}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
b=c(l[15],i,h),d=b[1],j=b[2];if(d){var
e=d[1];return[0,eH(c(p[22][9],e[1],e[2]),j)]}return 0}function
iV(r){function
s(i){var
b=i;for(;;){if(b){var
d=b[2],e=b[1],j=e[1],f=a(l[18],e[2]);try{var
k=function(g){return function(b){var
d=a(l[18],b[2]),e=q[2],f=c(q[17],g,d);return c(q[24],f,e)}}(f),h=c(g[33],k,d),m=h[1],n=[0,[0,[0,j,f],[0,m,a(l[18],h[2])]]];return n}catch(a){a=w(a);if(a===O){var
b=d;continue}throw a}}return 0}}function
t(b){var
a=b[1];return 0===a[2]?s(a[1]):0}var
b=c(l[15],t,r),d=b[1],u=b[2];if(d){var
e=d[1],h=e[2],i=h[1],j=e[1],k=j[2],m=j[1],v=h[2],x=k[1],y=m[1],n=cV(m[2],k[2]),z=[1,n[2]],A=[1,n[1]],o=function(d,b){var
a=c(p[7][17],d,b);return a?a[1]:sc};return[0,eH(function(g){var
b=g[1],d=b[1],h=g[2],j=b[3],k=b[2],l=o(y,d),m=o(x,d),n=c(f[6],m,z),q=c(f[6],l,A),r=c(f[1],q,n),e=a(f[3],r),s=c(f[6],e,i[3]),t=c(f[1],s,j),u=c(p[7][13],e,i[1]);return[0,[0,[0,c(p[7][14],u,d),k,t],[7,[4,[0,0,e],v],h]]]},u)]}return 0}function
sd(i){function
j(c){var
b=c[1];if(0===b[2])try{var
d=[0,a(g[5],b[1])[1]];return d}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
d=c(l[15],j,i),e=d[1],k=d[2];if(e){var
h=e[1],b=h[2],m=h[1];if(B[4]){var
o=a(f[40],b[1][3]);G(n[2],se,p[7][4],b[1][1],o)}return[0,eI(m,b,k)]}return 0}function
f6(e,d){var
b=d;for(;;){var
c=a(e,b);if(c){var
b=c[1];continue}return b}}function
f7(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1],d);if(c)return[0,c[1]];var
b=f;continue}return 0}}function
f8(a){var
b=[0,iT,[0,iS,[0,iV,0]]];return f6(function(a){return f7(b,a)},a)}function
iW(a){var
b=[0,iU,0];return f6(function(a){return f7(b,a)},a)}function
iX(d){function
C(a){return 0===a[2]?1:0}var
r=c(g[37],C,d),s=r[2],t=r[1];if(t)var
D=0,E=function(b,a){function
d(b){return c(p[7][1],a[1],b[1])}return c(g[28],d,t)?b:[0,a[1],b]},u=e(g[20],E,D,s);else
var
F=function(a){return a[1]},u=c(g[19],F,s);var
G=[0,p[7][3],sg];function
H(b,g){var
h=a(B[6][4],b[2]),m=h?c(f[29],h[1],sf):0;if(m)return b;var
j=c(B[47][1],g,d);if(j){var
k=j[1];if(B[4])e(n[2],sh,p[7][4],g);var
i=b[2],l=b[1];return c(B[6][5],k,i)?[0,g,k]:[0,l,i]}return b}var
v=e(g[20],H,G,u),w=v[2],x=w[1],I=v[1];if(x){var
y=w[2];if(y)var
b=[0,[0,x[1],I,y[1]]],o=1;else
var
o=0}else
var
o=0;if(!o)var
b=0;if(b){var
i=b[1],j=i[3],k=i[2],m=i[1],J=a(l[17],m),K=q[2],L=a(l[18],m),M=c(q[8],L,K),N=a(l[17],j),O=a(l[18],j),P=[1,c(q[5],q[2],O)],z=eF([0,[0,c(p[7][13],[1,N],k),1,P],d]),Q=a(f[3],[1,M]),R=a(f[3],[1,J]),A=eF([0,[0,c(p[7][13],R,k),1,Q],d]);if(z)if(A){var
S=A[1],T=a(g[6],z[1]);return[0,[0,a(g[6],S),[0,m,k,j],T]]}return a(h[2],si)}return 0}function
eJ(b){function
d(b){var
d=b[1][1];function
e(b){return 0!==a(f[25],b[2])?1:0}return c(g[27],e,d)}return c(g[27],d,b)}function
f9(o,m,b){function
q(j,b){if(B[4]){a(n[2],sj);a(h[51],h[27])}if(eJ(b)){var
k=a(g[46],b),l=k[2],m=iX(k[1]);if(m){var
c=m[1],d=c[2],o=d[3],e=d[2],q=d[1],s=c[3],t=c[1];if(B[4]){var
u=a(f[40],o),v=a(f[40],q);aX(n[2],sk,p[7][4],e,v,u)}var
w=a(f[22],o),r=i(j,e,a(f[24],q),w,b);if(r){var
x=r[1],y=eG(l,s);return[0,[1,j,eG(l,t),e,y,x]]}return 0}return 0}throw[0,D,sl]}function
i(b,g,a,e,d){if(c(f[28],a,e))return sm;var
h=j(b+1|0,[0,[0,[0,g,0,a],[1,b]],d]);if(h){var
l=h[1],k=i(b,g,c(f[1],a,sn),e,d);return k?[0,[0,l,k[1]]]:0}return 0}function
j(d,b){if(eJ(b)){if(B[4]){var
h=function(b,a){return c(p[9],b,a[1])},i=a(l[2],h);e(n[2],so,i,b)}try{var
f=a(m,b);if(B[4]){var
j=function(b,a){return c(p[9],b,a[1])},k=a(l[2],j);e(n[2],sp,k,f)}var
g=iM(f),r=g?[0,[0,d,g[1],0]]:o?q(d,f):0;return r}catch(a){a=w(a);if(a[1]===cU)return[0,[0,d,a[2],0]];throw a}}throw[0,D,sq]}var
k=a(g[1],b);try{var
t=j(k,iO(b)),d=t}catch(a){a=w(a);if(a[1]!==cU)throw a;var
d=[0,[0,k,a[2],0]]}if(d){var
r=d[1],s=function(b,a){return a};return[0,iK(c(l[23],s,b),r)]}return 0}function
f_(b){var
d=b[2],c=a(p[22][5],b[1]),e=c[1];return[0,e,d,a(f[3],c[2])]}function
sr(e,d,b){a(p[22][1][6],0);var
f=cS(b,d);B[10][1]=f;function
h(a){return f0(cQ,a)}var
i=c(g[17],h,b),j=c(g[17],f_,i);function
k(b,a){return[0,b,[0,a]]}return f9(e,f8,c(l[23],k,j))}var
aF=[0,qL,qM,qN,cQ,bt,cQ,cR,ir,is,it,iu,eC,iv,iw,ix,q7,eD,iy,ra,fY,re,fZ,aM,iz,iA,iB,iC,cS,rl,rm,rn,rp,rq,rr,rs,rt,iE,iF,ru,f0,iG,rw,iH,ry,f1,rD,bO,aN,f2,eE,rJ,aO,f3,rN,iJ,f4,aP,cT,iK,eF,rW,rX,eG,iM,f5,iN,cU,iO,cV,r5,r8,r9,iP,iQ,iR,eH,eI,iS,iT,iU,iV,sd,f6,f7,f8,iW,iX,eJ,f9,f_,sr,function(n,m,f){a(p[22][1][6],0);var
o=cS(f,m);B[10][1]=o;function
q(a){return f0(cQ,a)}var
r=c(g[17],q,f);function
s(b,a){return[0,b,[0,a]]}var
b=c(l[23],s,r);function
t(b){return a(p[6][13],b[1][1])}var
i=c(g[27],t,b),u=aM[1];function
v(c,b){var
d=b[1][1];function
f(c,f,b){var
d=a(p[5][13],c);return d?e(aM[4],d[1],c,b):b}return e(p[6][9],f,d,c)}var
w=e(g[20],v,u,b);function
x(d,c,b){var
f=a(p[6][4],ss),g=e(p[6][3],d,st,f),h=a(p[22][5],g),i=a(p[6][4],su);return[0,[0,[0,e(p[6][3],c,sv,i),1],[3,h]],b]}var
d=e(aM[13],x,w,b);if(i)var
j=d;else
var
A=function(b,a){var
d=a[1],e=b[1],f=d[1],g=e[1],h=[6,b[2],a[2]],i=c(p[10],e[2],d[2]);return[0,[0,c(p[6][6],g,f),i],h]},C=c(l[8],A,d),j=c(h[25],d,C);function
y(a){var
b=a[2];return[0,f_(a[1]),b]}var
k=c(g[17],y,j);if(eJ(k)){var
z=i?f8:iW;return f9(n,z,k)}throw[0,D,sw]}];aY(746,aF,"Micromega_plugin.Certificate");var
cW=[0,function(p){var
b=a(bn[19],p),l=[a0,sx,aZ(0)],f=[a0,sy,aZ(0)];function
q(d,c){var
f=a(b[1],d),g=e(E[23],c,sz,e3);return[0,a(E[31],g),1,f]}function
r(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
s(b){try{var
c=[0,a(ee[3],b)];return c}catch(b){b=w(b);if(b===sA)return 0;if(a(br[20],b))throw l;throw b}}function
t(c,a){var
d=e(E[34],a,0,1);try{e(E[34],a,0,0);var
f=0===c?4:1;e(E[83],a,f,1);var
g=1,b=g}catch(a){a=w(a);if(a[1]!==E[1])throw a;var
b=0}e(E[34],a,d,0);return b}function
u(a){var
c=e(E[34],a,0,1);try{e(E[34],a,0,0);var
b=e(E[83],a,0,1);return b}catch(b){b=w(b);if(b[1]===E[1]){e(E[34],a,c,0);return 0}throw b}}function
g(d,c,b){return t(d,c)?r(b,function(a){return u(c)}):a(b,0)}function
m(i){var
f=e(E[23],i,sB,e3),j=a(E[30],f),d=a(b[1],gN);function
n(f){for(;;){var
a=s(j);if(a){var
c=a[1];e(b[5],d,c[1],c[2]);continue}return 0}}try{g(0,f,n);a(h[82],j);var
o=e(E[23],i,sE,e3),p=[0,a(E[31],o),1,d];return p}catch(f){f=w(f);if(f===l){a(h[82],j);var
m=e(E[23],i,sC,e3),k=a(E[31],m);g(1,m,function(g){function
f(b,a){return e(ee[1],k,[0,b,a],sD)}c(b[12],f,d);return a(h[51],k)});return[0,k,1,d]}throw f}}function
v(c){var
d=c[1],e=c[3];return 0===c[2]?0:(a(h[64],d),a(b[2],e),c[2]=0,0)}function
n(c,j,i){var
d=c[1],k=c[3];if(0===c[2])throw f;var
l=a(E[33],d);e(b[5],k,j,i);return g(1,l,function(b){e(ee[1],d,[0,j,i],sF);return a(h[51],d)})}function
o(a,d){var
e=a[3];if(0===a[2])throw f;return c(b[7],e,d)}return[0,q,m,o,n,v,function(c,e){var
b=[d,function(b){try{var
a=[0,m(c)];return a}catch(a){return 0}}];return function(c){var
f=j(b),g=k===f?b[1]:d===f?a(i[2],b):b;if(g){var
h=g[1];try{var
m=o(h,c);return m}catch(b){b=w(b);if(b===O){var
l=a(e,c);n(h,c,l);return l}throw b}}return a(e,c)}}]}];aY(749,cW,"Micromega_plugin.Persistent_cache");var
sG=0;function
sH(d,c,b){var
f=a(E[97],0)[1],g=a(c,b),i=a(E[97],0)[1]-f;e(n[2],sI,d,i);a(h[51],h[27]);return g}var
eK=h[7],f$=[0,eK],eL=[0,1],ga=[0,eK];function
gb(a){return[0,eL[1],ga[1]]}function
eM(a){return f$[1]}function
iY(b,a){function
c(b){var
c=b?b[1]:eK;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,e(g[21],h[16],b,sJ),b,d,c]}function
sK(a){eL[1]=a;return 0}var
sN=[0,0,sM,sL,function(a){return eL[1]},sK],sP=iY(sO,f$);c(gc[3],0,sP);var
sR=iY(sQ,ga);c(gc[3],0,sR);c(gc[4],0,sN);function
bu(d,b){if(typeof
b==="number")return 0===b?c(h[54],d,sS):c(h[54],d,sT);else
switch(b[0]){case
0:return c(h[54],d,sU);case
1:return G(n[1],d,sV,l[32][3],b[2]);case
2:return C(n[1],d,sW,bu,b[1],bu,b[2]);case
3:return C(n[1],d,sX,bu,b[1],bu,b[2]);case
4:return G(n[1],d,sY,bu,b[1]);default:var
e=b[2],f=b[3],g=b[1],i=e?a(H[1][8],e[1]):s0;return d5(n[1],d,sZ,bu,g,i,bu,f)}}function
aQ(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
d=b[3],e=b[2];return[1,a(c,b[1]),e,d];case
2:var
f=b[1],g=aQ(c,b[2]);return[2,aQ(c,f),g];case
3:var
h=b[1],i=aQ(c,b[2]);return[3,aQ(c,h),i];case
4:return[4,aQ(c,b[1])];default:var
j=b[2],k=b[1],l=aQ(c,b[3]);return[5,aQ(c,k),j,l]}}function
a7(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1],b[2],b[3]];case
2:var
d=b[1],e=a7(c,b[2]);return[2,a7(c,d),e];case
3:var
f=b[1],g=a7(c,b[2]);return[3,a7(c,f),g];case
4:return[4,a7(c,b[1])];default:var
h=b[2],i=b[1],j=a7(c,b[3]);return[5,a7(c,i),h,j]}}function
gd(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];if(b){var
c=b[1];return[0,c,gd(a[3])]}}return 0}var
eN=0,s1=0;function
eO(I,H,m,l,b){function
j(b,a){return c(h[25],b,a)}function
k(b,e){if(e){var
h=e[2],d=e[1],i=c(l,b[1],d[1]);if(i){if(a(m,i[1]))return[0,[0,b[2],[0,d[2],0]]];var
f=k(b,h);return 0===f[0]?[0,f[1]]:[1,[0,d,f[1]]]}var
g=k(b,h);return 0===g[0]?[0,g[1]]:[1,[0,d,g[1]]]}var
j=c(l,b[1],b[1]);return j?a(m,j[1])?[0,[0,b[2],0]]:[1,[0,b,0]]:[1,[0,b,0]]}function
i(a,d){if(a){var
m=a[1],f=i(a[2],d),l=f[2],n=f[1],j=function(l,f){var
g=f[2],i=f[1],a=m,d=l;for(;;){if(a){var
j=a[2],e=k(a[1],d);if(0!==e[0]){var
a=j,d=e[1];continue}var
b=[0,e[1]]}else
var
b=[1,d];return 0===b[0]?[0,i,c(h[25],g,b[1])]:[0,[0,b[1],i],g]}},b=e(g[21],j,d,s2),o=b[1],p=c(h[25],l,b[2]);return[0,c(h[25],n,o),p]}return[0,eN,0]}function
f(O,N){var
b=O,d=N;for(;;)if(typeof
d==="number")return 0===d?b?[0,eN,0]:[0,cX,0]:b?[0,cX,0]:[0,eN,0];else
switch(d[0]){case
0:return b?[0,cX,0]:[0,cX,0];case
1:var
e=d[2],k=d[1],P=0;if(b)var
L=a(H,k),M=function(a){function
b(a){return[0,a,e]}return c(g[17],b,a)},l=c(g[17],M,L);else
var
J=a(I,k),K=function(a){function
b(a){return[0,a,e]}return c(g[17],b,a)},l=c(g[17],K,J);return[0,l,P];case
2:var
Q=d[2],m=f(b,d[1]),n=m[2],o=m[1],p=f(b,Q),q=p[2],r=p[1];if(b){var
R=c(h[25],n,q);return[0,j(o,r),R]}var
s=i(o,r),S=s[1],T=c(h[25],q,s[2]);return[0,S,c(h[25],n,T)];case
3:var
U=d[2],t=f(b,d[1]),u=t[2],v=t[1],w=f(b,U),x=w[2],y=w[1];if(b){var
z=i(v,y),V=z[1],W=c(h[25],x,z[2]);return[0,V,c(h[25],u,W)]}var
X=c(h[25],u,x);return[0,j(v,y),X];case
4:var
b=1-b,d=d[1];continue;default:var
Y=d[3],A=f(1-b,d[1]),B=A[2],C=A[1],D=f(b,Y),E=D[2],F=D[1];if(b){var
G=i(C,F),Z=G[1],_=c(h[25],E,G[2]);return[0,Z,c(h[25],B,_)]}var
$=c(h[25],B,E);return[0,j(C,F),$]}}return f(1,b)}var
ar=a(ec[1],[0,aa]),s3=a(bJ[1],[0,aa]);function
iZ(f,a){function
d(h,g){var
b=h,a=g;for(;;){if(a){var
e=a[2],i=a[1];if(c(ar[3],b,f))return[0,i,d(b+1|0,e)];var
b=b+1|0,a=e;continue}return 0}}return d(0,a)}var
s4=c(h[25],cY[6],ge),s5=c(h[25],cY[5],s4),s6=c(h[25],[0,i0,0],s5),i1=c(h[25],cY[7],s6);function
N(d,c,b){var
f=e(cY[4],d,c,b),g=a(s7[45],f);return a(m[8],g)}var
s8=cY[7];function
am(a){return N(s9,s8,a)}function
u(a){return N(s_,i1,a)}function
aG(a){return N(s$,i2,a)}function
an(a){return N(ta,i3,a)}function
aR(a){return N(tb,i4,a)}function
aS(a){return N(tc,ge,a)}var
a8=[d,function(a){return am(td)}],a9=[d,function(a){return am(te)}],cZ=[d,function(a){return am(tf)}],c0=[d,function(a){return am(tg)}],a_=[d,function(a){return am(th)}],as=[d,function(a){return am(ti)}],c1=[d,function(a){return u(tj)}],c2=[d,function(a){return u(tk)}],tm=[d,function(a){return u(tl)}],c3=[d,function(a){return am(tn)}],c4=[d,function(a){return am(to)}],tq=[d,function(a){return am(tp)}],c5=[d,function(a){return aG(tr)}],c6=[d,function(a){return aG(ts)}],c7=[d,function(a){return am(tt)}],tv=[d,function(a){return am(tu)}],tx=[d,function(a){return am(tw)}],tz=[d,function(a){return aG(ty)}],a$=[d,function(a){return aG(tA)}],ba=[d,function(a){return aG(tB)}],bb=[d,function(a){return aG(tC)}],bc=[d,function(a){return aG(tD)}],c8=[d,function(a){return aG(tE)}],c9=[d,function(a){return aG(tF)}],c_=[d,function(a){return aG(tG)}],c$=[d,function(a){return u(tH)}],bd=[d,function(a){return u(tI)}],tK=[d,function(a){return u(tJ)}],at=[d,function(a){return u(tL)}],tN=[d,function(a){return u(tM)}],da=[d,function(a){return aS(tO)}],db=[d,function(a){return aS(tP)}],dc=[d,function(a){return aS(tQ)}],dd=[d,function(a){return aS(tR)}],de=[d,function(a){return aS(tS)}],df=[d,function(a){return aS(tT)}],dg=[d,function(a){return aS(tU)}],dh=[d,function(a){return aS(tV)}],di=[d,function(a){return aS(tW)}],au=[d,function(a){return u(tX)}],av=[d,function(a){return u(tY)}],t0=[d,function(a){return u(tZ)}],t2=[d,function(a){return u(t1)}],t4=[d,function(a){return u(t3)}],t6=[d,function(a){return u(t5)}],t8=[d,function(a){return u(t7)}],i5=[d,function(a){return aR(t9)}],i6=[d,function(a){return aR(t_)}],i7=[d,function(a){return aR(t$)}],i8=[d,function(a){return aR(ua)}],aw=[d,function(a){return am(ub)}],b7=[d,function(a){return aR(uc)}],b8=[d,function(a){return aR(ud)}],b9=[d,function(a){return aR(ue)}],b_=[d,function(a){return aR(uf)}],b$=[d,function(a){return aR(ug)}],ui=[d,function(a){return u(uh)}],uk=[d,function(a){return u(uj)}],i9=[d,function(a){return u(ul)}],i_=[d,function(a){return u(um)}],i$=[d,function(a){return u(un)}],ca=[d,function(a){return u(uo)}],cb=[d,function(a){return u(up)}],cc=[d,function(a){return u(uq)}],cd=[d,function(a){return u(ur)}],ce=[d,function(a){return u(us)}],ja=[d,function(a){return an(ut)}],jb=[d,function(a){return an(uu)}],jc=[d,function(a){return an(uv)}],jd=[d,function(a){return an(uw)}],af=[d,function(a){return an(ux)}],aH=[d,function(a){return an(uy)}],aT=[d,function(a){return an(uz)}],ag=[d,function(a){return an(uA)}],uC=[d,function(a){return an(uB)}],be=[d,function(a){return an(uD)}],cf=[d,function(a){return an(uE)}],bf=[d,function(a){return an(uF)}],bg=[d,function(a){return an(uG)}],dj=[d,function(a){return u(uH)}],dk=[d,function(a){return u(uI)}],dl=[d,function(a){return u(uJ)}],dm=[d,function(a){return u(uK)}],dn=[d,function(a){return u(uL)}],dp=[d,function(a){return u(uM)}],dq=[d,function(a){return u(uN)}],dr=[d,function(a){return u(uO)}],ds=[d,function(a){return u(uP)}],dt=[d,function(a){return u(uQ)}],du=[d,function(a){return u(uR)}],dv=[d,function(a){return u(uS)}],dw=[d,function(a){return u(uT)}],dx=[d,function(a){return u(uU)}],dy=[d,function(a){return u(uV)}],dz=[d,function(a){return u(uW)}],dA=[d,function(a){return u(uX)}],dB=[d,function(a){return u(uY)}],dC=[d,function(a){return u(uZ)}],dD=[d,function(a){return u(u0)}],dE=[d,function(a){return u(u1)}],dF=[d,function(a){return u(u2)}],dG=[d,function(a){return u(u3)}],u5=[d,function(a){return u(u4)}],u9=[d,function(a){return N(u8,u7,u6)}],vb=[d,function(a){return N(va,u$,u_)}],dH=[d,function(a){return N(ve,vd,vc)}],dI=[d,function(a){return N(vh,vg,vf)}],dJ=[d,function(a){return N(vk,vj,vi)}],dK=[d,function(a){return N(vn,vm,vl)}],dL=[d,function(a){return N(vq,vp,vo)}],dM=[d,function(a){return N(vt,vs,vr)}],dN=[d,function(a){return N(vw,vv,vu)}],dO=[d,function(a){return N(vz,vy,vx)}],vD=[d,function(a){return N(vC,vB,vA)}],vH=[d,function(a){return N(vG,vF,vE)}],vL=[d,function(a){return N(vK,vJ,vI)}],vP=[d,function(a){return N(vO,vN,vM)}],dP=[d,function(a){return N(vS,vR,vQ)}],vW=[d,function(a){return N(vV,vU,vT)}];function
vX(b){if(typeof
b==="number")return vY;else
switch(b[0]){case
0:return b[1];case
1:return a(h[21],b[1]);case
2:return vZ;case
3:return b[1];default:return v0}}var
P=[a0,v1,aZ(0)];function
cg(b,e){var
a=c(m[3],b,e);switch(a[0]){case
9:var
f=a[2],d=c(m[3],b,a[1]);if(12===d[0])return[0,d[1][1][2],f];throw P;case
12:return[0,a[1][1][2],[0]];default:throw P}}function
gf(a,d){var
b=cg(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,gf(a,y(e,0)[1])];throw P}function
je(c,b){var
d=a(l[29][1],b);return e(n[1],c,v2,d)}function
eP(b){if(b){var
c=j(c4),f=[0,eP(b[1])],g=k===c?c4[1]:d===c?a(i[2],c4):c4;return a(m[21],[0,g,f])}var
e=j(c3);return k===e?c3[1]:d===e?a(i[2],c3):c3}function
ch(a,e){var
b=cg(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw P;switch(d){case
0:return[0,ch(a,y(c,0)[1])];case
1:return[1,ch(a,y(c,0)[1])];default:return 0}}function
ax(b){if(typeof
b==="number"){var
c=j(a$);return k===c?a$[1]:d===c?a(i[2],a$):a$}else{if(0===b[0]){var
e=j(bb),g=[0,ax(b[1])],h=k===e?bb[1]:d===e?a(i[2],bb):bb;return a(m[21],[0,h,g])}var
f=j(ba),l=[0,ax(b[1])],n=k===f?ba[1]:d===f?a(i[2],ba):ba;return a(m[21],[0,n,l])}}function
dQ(c,b){var
d=a(l[29][2],b);return e(n[1],c,v3,d)}function
jf(b){if(b){var
c=j(c6),f=[0,ax(b[1])],g=k===c?c6[1]:d===c?a(i[2],c6):c6;return a(m[21],[0,g,f])}var
e=j(c5);return k===e?c5[1]:d===e?a(i[2],c5):c5}function
gg(b){if(typeof
b==="number"){var
c=j(a$);return k===c?a$[1]:d===c?a(i[2],a$):a$}else{if(0===b[0]){var
e=j(bb),g=[0,gg(b[1])],h=k===e?bb[1]:d===e?a(i[2],bb):bb;return a(m[21],[0,h,g])}var
f=j(ba),l=[0,gg(b[1])],n=k===f?ba[1]:d===f?a(i[2],ba):ba;return a(m[21],[0,n,l])}}function
v4(c,b){var
d=a(l[29][4],b);return e(n[1],c,v5,d)}function
jg(d,b){var
e=a(l[29][3],b),f=a(h[21],e);return c(h[54],d,f)}function
v6(h,g,f,e,b){var
l=b[1],n=a(e,b[2]),c=j(c7),o=[0,h,g,a(f,l),n],p=k===c?c7[1]:d===c?a(i[2],c7):c7;return a(m[21],[0,p,o])}function
bP(a,e){var
b=cg(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw P;switch(d){case
0:return 0;case
1:return[0,ch(a,y(c,0)[1])];default:return[1,ch(a,y(c,0)[1])]}}function
bv(b){if(typeof
b==="number"){var
c=j(c8);return k===c?c8[1]:d===c?a(i[2],c8):c8}else{if(0===b[0]){var
e=j(c9),g=[0,ax(b[1])],h=k===e?c9[1]:d===e?a(i[2],c9):c9;return a(m[21],[0,h,g])}var
f=j(c_),l=[0,ax(b[1])],n=k===f?c_[1]:d===f?a(i[2],c_):c_;return a(m[21],[0,n,l])}}function
jh(c,b){var
d=a(l[29][7],b),f=a(q[33],d);return e(n[1],c,v7,f)}function
v8(b){var
e=a(l[17],b),f=ax(a(l[30][6],e)),g=a(l[18],b),c=j(at),h=[0,bv(a(l[30][7],g)),f],n=k===c?at[1]:d===c?a(i[2],at):at;return a(m[21],[0,n,h])}function
eQ(b){var
e=ax(b[2]),c=j(at),f=[0,bv(b[1]),e],g=k===c?at[1]:d===c?a(i[2],at):at;return a(m[21],[0,g,f])}function
dR(b,l){var
f=c(m[3],b,l);if(9===f[0]){var
g=f[2],h=j(at),n=f[1],o=k===h?at[1]:d===h?a(i[2],at):at;if(e(m[93],b,n,o)){var
p=ch(b,y(g,1)[2]);return[0,bP(b,y(g,0)[1]),p]}throw P}throw P}function
bh(b,a){if(typeof
a==="number")return 0===a?c(h[54],b,v9):c(h[54],b,v_);else
switch(a[0]){case
0:return c(h[54],b,v$);case
1:return jh(b,a[1]);case
2:return C(n[1],b,wa,bh,a[1],bh,a[2]);case
3:return C(n[1],b,wb,bh,a[1],bh,a[2]);case
4:return C(n[1],b,wc,bh,a[1],bh,a[2]);case
5:return G(n[1],b,wd,bh,a[1]);default:return G(n[1],b,we,bh,a[1])}}function
bi(b){if(typeof
b==="number"){if(0===b){var
c=j(da);return k===c?da[1]:d===c?a(i[2],da):da}var
e=j(db);return k===e?db[1]:d===e?a(i[2],db):db}else
switch(b[0]){case
0:var
f=j(dc),q=[0,eQ(b[1])],r=k===f?dc[1]:d===f?a(i[2],dc):dc;return a(m[21],[0,r,q]);case
1:var
g=j(dd),s=[0,bv(b[1])],t=k===g?dd[1]:d===g?a(i[2],dd):dd;return a(m[21],[0,t,s]);case
2:var
u=b[1],v=bi(b[2]),h=j(de),w=[0,bi(u),v],x=k===h?de[1]:d===h?a(i[2],de):de;return a(m[21],[0,x,w]);case
3:var
y=b[1],z=bi(b[2]),l=j(df),A=[0,bi(y),z],B=k===l?df[1]:d===l?a(i[2],df):df;return a(m[21],[0,B,A]);case
4:var
C=b[1],D=bi(b[2]),n=j(dg),E=[0,bi(C),D],F=k===n?dg[1]:d===n?a(i[2],dg):dg;return a(m[21],[0,F,E]);case
5:var
o=j(dh),G=[0,bi(b[1])],H=k===o?dh[1]:d===o?a(i[2],dh):dh;return a(m[21],[0,H,G]);default:var
p=j(di),I=[0,bi(b[1])],J=k===p?di[1]:d===p?a(i[2],di):di;return a(m[21],[0,J,I])}}function
bj(a,e){var
c=cg(a,e),b=c[2],d=c[1]-1|0;if(7<d>>>0)throw P;switch(d){case
0:return 0;case
1:return 1;case
2:return[0,dR(a,y(b,0)[1])];case
3:var
f=bj(a,y(b,1)[2]);return[2,bj(a,y(b,0)[1]),f];case
4:var
g=bj(a,y(b,1)[2]);return[3,bj(a,y(b,0)[1]),g];case
5:var
h=bj(a,y(b,1)[2]);return[4,bj(a,y(b,0)[1]),h];case
6:return[5,bj(a,y(b,0)[1])];default:return[6,bj(a,y(b,0)[1])]}}function
ji(a,b,g){var
d=cg(a,g),e=d[2],f=d[1];if(1===f)return 0;if(2===f){var
h=ji(a,b,y(e,2)[3]);return[0,c(b,a,y(e,1)[2]),h]}throw P}function
jj(c,e,b){if(b){var
h=b[1],l=jj(c,e,b[2]),f=j(c1),n=[0,c,a(e,h),l],o=k===f?c1[1]:d===f?a(i[2],c1):c1;return a(m[21],[0,o,n])}var
g=j(c2),p=[0,c],q=k===g?c2[1]:d===g?a(i[2],c2):c2;return a(m[21],[0,q,p])}function
wf(f,e,b,d,a){function
c(d,a){if(a){var
e=a[2],f=a[1];return e?C(n[1],d,wg,b,f,c,e):G(n[1],d,wh,b,f)}return 0}return C(n[1],d,wi,f,c,a,e)}function
gh(e,d,a){function
b(d,a){switch(a[0]){case
0:return c(e,d,a[1]);case
1:return G(n[1],d,wj,dQ,a[1]);case
2:return C(n[1],d,wk,b,a[1],b,a[2]);case
3:return C(n[1],d,wl,b,a[1],b,a[2]);case
4:return C(n[1],d,wm,b,a[1],b,a[2]);case
5:return G(n[1],d,wn,b,a[1]);default:return C(n[1],d,wo,b,a[1],jg,a[2])}}return b(d,a)}function
gi(e,q,b){function
c(b){switch(b[0]){case
0:var
f=j(dk),r=[0,e,a(q,b[1])],s=k===f?dk[1]:d===f?a(i[2],dk):dk;return a(m[21],[0,s,r]);case
1:var
g=j(dj),t=[0,e,ax(b[1])],u=k===g?dj[1]:d===g?a(i[2],dj):dj;return a(m[21],[0,u,t]);case
2:var
v=b[1],w=c(b[2]),h=j(dl),x=[0,e,c(v),w],y=k===h?dl[1]:d===h?a(i[2],dl):dl;return a(m[21],[0,y,x]);case
3:var
z=b[1],A=c(b[2]),l=j(dp),B=[0,e,c(z),A],C=k===l?dp[1]:d===l?a(i[2],dp):dp;return a(m[21],[0,C,B]);case
4:var
D=b[1],E=c(b[2]),n=j(dn),F=[0,e,c(D),E],G=k===n?dn[1]:d===n?a(i[2],dn):dn;return a(m[21],[0,G,F]);case
5:var
o=j(dm),H=[0,e,c(b[1])],I=k===o?dm[1]:d===o?a(i[2],dm):dm;return a(m[21],[0,I,H]);default:var
J=b[1],K=jf(b[2]),p=j(dq),L=[0,e,c(J),K],M=k===p?dq[1]:d===p?a(i[2],dq):dq;return a(m[21],[0,M,L])}}return c(b)}function
gj(e,l,b){function
c(b){switch(b[0]){case
0:var
f=j(ds),n=[0,e,a(l,b[1])],o=k===f?ds[1]:d===f?a(i[2],ds):ds;return a(m[21],[0,o,n]);case
1:var
p=b[1],q=c(b[2]),g=j(dt),r=[0,e,ax(p),q],s=k===g?dt[1]:d===g?a(i[2],dt):dt;return a(m[21],[0,s,r]);default:var
t=b[2],u=b[1],v=c(b[3]),w=ax(t),h=j(dr),x=[0,e,c(u),w,v],y=k===h?dr[1]:d===h?a(i[2],dr):dr;return a(m[21],[0,y,x])}}return c(b)}function
eR(d,c,a){function
b(c,a){switch(a[0]){case
0:return G(n[1],c,wp,d,a[1]);case
1:return C(n[1],c,wq,dQ,a[1],b,a[2]);default:return bS(n[1],c,wr,b,a[1],dQ,a[2],b,a[3])}}return b(c,a)}function
ws(d,b,a){function
e(b,a){function
e(a){var
c=a[2],e=a[1][1],f=l[32][3];function
g(a,b){return eR(d,a,b)}return C(n[1],b,wt,g,e,f,c)}return c(g[15],e,a)}function
f(a){return G(n[1],b,wu,e,a)}return c(g[15],f,a)}function
wv(b,f,h){var
g=j(b),c=k===g?b[1]:d===g?a(i[2],b):b;function
e(b){if(typeof
b==="number"){var
g=j(dG),r=[0,c],s=k===g?dG[1]:d===g?a(i[2],dG):dG;return a(m[21],[0,s,r])}else
switch(b[0]){case
0:var
h=j(dA),t=[0,c,eP(b[1])],u=k===h?dA[1]:d===h?a(i[2],dA):dA;return a(m[21],[0,u,t]);case
1:var
l=j(dB),v=[0,c,gj(c,f,b[1])],w=k===l?dB[1]:d===l?a(i[2],dB):dB;return a(m[21],[0,w,v]);case
2:var
x=b[1],y=e(b[2]),n=j(dD),z=[0,c,gj(c,f,x),y],A=k===n?dD[1]:d===n?a(i[2],dD):dD;return a(m[21],[0,A,z]);case
3:var
B=b[1],C=e(b[2]),o=j(dC),D=[0,c,e(B),C],E=k===o?dC[1]:d===o?a(i[2],dC):dC;return a(m[21],[0,E,D]);case
4:var
F=b[1],G=e(b[2]),p=j(dE),H=[0,c,e(F),G],I=k===p?dE[1]:d===p?a(i[2],dE):dE;return a(m[21],[0,I,H]);default:var
q=j(dF),J=[0,c,a(f,b[1])],K=k===q?dF[1]:d===q?a(i[2],dF):dF;return a(m[21],[0,K,J])}}return e(h)}function
ww(e,b,a){function
d(b,a){if(typeof
a==="number")return c(n[1],b,wx);else
switch(a[0]){case
0:return G(n[1],b,wy,je,a[1]);case
1:var
f=a[1],g=function(a,b){return eR(e,a,b)};return G(n[1],b,wz,g,f);case
2:var
h=a[2],i=a[1],j=function(a,b){return eR(e,a,b)};return C(n[1],b,wA,j,i,d,h);case
3:return C(n[1],b,wB,d,a[1],d,a[2]);case
4:return C(n[1],b,wC,d,a[1],d,a[2]);default:return G(n[1],b,wD,e,a[1])}}return d(b,a)}function
jk(l){switch(l){case
0:var
b=j(du);return k===b?du[1]:d===b?a(i[2],du):du;case
1:var
c=j(dv);return k===c?dv[1]:d===c?a(i[2],dv):dv;case
2:var
e=j(dw);return k===e?dw[1]:d===e?a(i[2],dw):dw;case
3:var
f=j(dy);return k===f?dy[1]:d===f?a(i[2],dy):dy;case
4:var
g=j(dx);return k===g?dx[1]:d===g?a(i[2],dx):dx;default:var
h=j(dz);return k===h?dz[1]:d===h?a(i[2],dz):dz}}function
jl(a,b){switch(b){case
0:return c(n[1],a,wE);case
1:return c(n[1],a,wF);case
2:return c(n[1],a,wG);case
3:return c(n[1],a,wH);case
4:return c(n[1],a,wI);default:return c(n[1],a,wJ)}}function
wK(b,c,a){var
d=a[3],e=a[2],f=a[1];function
g(a,c){return gh(b,a,c)}function
h(a,c){return gh(b,a,c)}return bS(n[1],c,wL,h,f,jl,e,g,d)}function
wM(c,e,b){var
g=b[2],h=b[1],l=gi(c,e,b[3]),n=jk(g),f=j(dP),o=[0,c,gi(c,e,h),n,l],p=k===f?dP[1]:d===f?a(i[2],dP):dP;return a(m[21],[0,p,o])}function
dS(h,f,b){try{var
l=function(g){var
b=g[1],c=j(b),l=k===c?b[1]:d===c?a(i[2],b):b;return e(m[93],h,f,l)},n=c(g[33],l,b)[2];return n}catch(a){a=w(a);if(a===O)throw P;throw a}}var
gk=[0,[0,i5,5],[0,[0,i6,3],[0,[0,i8,4],[0,[0,i7,2],0]]]],gl=[0,[0,ja,5],[0,[0,jb,3],[0,[0,jd,4],[0,[0,jc,2],0]]]],gm=[0,[0,i_,4],[0,[0,i9,2],[0,[0,i$,0],0]]];function
gn(a,c,b){return aX(wN[81],0,a[1],a[2],c,b)}function
jm(n,l){var
b=l[2],f=l[1],g=n[2],o=c(m[3],g,f);switch(o[0]){case
10:var
r=y(b,1)[2],s=y(b,0)[1];return[0,dS(g,f,gk),s,r];case
11:if(0===o[1][1][2]){var
p=j(aw),t=k===p?aw[1]:d===p?a(i[2],aw):aw;if(e(m[93],g,f,t)){var
q=j(bc),u=k===q?bc[1]:d===q?a(i[2],bc):bc;if(gn(n,y(b,0)[1],u)){var
v=y(b,2)[3];return[0,0,y(b,1)[2],v]}}throw P}break}return a(h[2],wO)}function
jn(n,l){var
b=l[2],f=l[1],g=n[2],o=c(m[3],g,f);switch(o[0]){case
10:var
r=y(b,1)[2],s=y(b,0)[1];return[0,dS(g,f,gl),s,r];case
11:if(0===o[1][1][2]){var
p=j(aw),t=k===p?aw[1]:d===p?a(i[2],aw):aw;if(e(m[93],g,f,t)){var
q=j(bd),u=k===q?bd[1]:d===q?a(i[2],bd):bd;if(gn(n,y(b,0)[1],u)){var
v=y(b,2)[3];return[0,0,y(b,1)[2],v]}}throw P}break}return a(h[2],wP)}function
jo(c,a){var
b=a[2],d=a[1],e=y(b,1)[2],f=y(b,0)[1];return[0,dS(c[2],d,gm),f,e]}function
wQ(b,a){return 12===c(m[3],b,a)[0]?1:0}function
jp(h,f,b){try{var
l=function(g){var
b=g[1],c=j(b),l=k===c?b[1]:d===c?a(i[2],b):b;return e(m[93],h,f,l)},n=c(g[33],l,b)[2];return n}catch(a){a=w(a);if(a===O)return wR;throw a}}function
wS(f,h,c){function
d(a,c,b){if(a){var
f=a[1],i=a[2];if(e(m[93],h,f,b))return[0,a,c];var
g=d(i,c+1|0,b);return[0,[0,f,g[1]],g[2]]}return[0,[0,b,0],c]}var
b=d(f,1,c),g=b[1];return[0,g,a(l[30][2],b[2])]}function
wT(f,d,c){var
a=f,b=1;for(;;){if(a){var
g=a[2];if(e(m[93],d,a[1],c))return b;var
a=g,b=b+1|0;continue}throw[0,jq,wU]}}var
wV=0,bQ=[0,wS,wT,wV,function(a){return a}];function
eS(d,i,t,s,f,b){function
l(c,b){var
a=e(bQ[1],c,d,b);return[0,[1,a[2]],a[1]]}function
g(b,f){function
u(f,e,a){var
h=a[2],b=g(f,a[1]),i=b[1],d=g(b[2],h),j=d[2];return[0,c(e,i,d[1]),j]}try{var
C=[0,[0,a(i,f)],b];return C}catch(i){i=w(i);if(i===P){var
k=c(m[3],d,f);if(9===k[0]){var
h=k[2],n=k[1];if(10===c(m[3],d,n)[0]){var
j=jp(d,n,s);if(typeof
j==="number"){if(0===j){var
o=g(b,y(h,0)[1]);return[0,[5,o[1]],o[2]]}try{var
q=g(b,y(h,0)[1]),v=q[2],x=q[1],z=[0,c(t,x,y(h,1)[2]),v];return z}catch(c){c=w(c);if(a(br[20],c)){var
p=e(bQ[1],b,d,f);return[0,[1,p[2]],p[1]]}throw c}}else{if(0===j[0]){var
A=j[1],B=y(h,1)[2];return u(b,A,[0,y(h,0)[1],B])}var
r=e(bQ[1],b,d,f);return[0,[1,r[2]],r[1]]}}return l(b,f)}return l(b,f)}throw i}}return g(f,b)}var
wW=[0,[0,b9,0],[0,[0,b$,1],0]],wX=[0,[0,b_,[0,function(b,a){return[4,b,a]}]],wW],wY=[0,[0,b8,[0,function(b,a){return[3,b,a]}]],wX],jr=[0,[0,b7,[0,function(b,a){return[2,b,a]}]],wY],wZ=[0,[0,cc,0],[0,[0,ce,1],0]],w0=[0,[0,cd,[0,function(b,a){return[4,b,a]}]],wZ],w1=[0,[0,cb,[0,function(b,a){return[3,b,a]}]],w0],js=[0,[0,ca,[0,function(b,a){return[2,b,a]}]],w1],w2=[0,[0,aT,0],[0,[0,cf,1],0]],w3=[0,[0,ag,[0,function(b,a){return[4,b,a]}]],w2],w4=[0,[0,aH,[0,function(b,a){return[3,b,a]}]],w3],jt=[0,[0,af,[0,function(b,a){return[2,b,a]}]],w4],w5=0,w6=[0,[0,ag,function(b,a){return[4,b,a]}],w5],w7=[0,[0,aH,function(b,a){return[3,b,a]}],w6],ju=[0,[0,af,function(b,a){return[2,b,a]}],w7];function
eT(b,h){var
l=c(m[3],b,h);switch(l[0]){case
9:var
f=l[2],g=l[1];try{var
A=dS(b,g,ju),B=eT(b,y(f,0)[1]),C=c(A,B,eT(b,y(f,1)[2]));return C}catch(h){h=w(h);if(h===P){var
n=j(be),u=k===n?be[1]:d===n?a(i[2],be):be;if(e(m[93],b,g,u)){var
o=eT(b,y(f,0)[1]),v=a(s[gQ],o);if(c(s[77],v,w8))throw P;return[5,o]}var
p=j(bg),x=k===p?bg[1]:d===p?a(i[2],bg):bg;if(e(m[93],b,g,x))return[0,dR(b,y(f,0)[1])];var
q=j(bf),z=k===q?bf[1]:d===q?a(i[2],bf):bf;if(e(m[93],b,g,z))return[1,bP(b,y(f,0)[1])];throw P}throw h}case
10:var
r=j(au),D=k===r?au[1]:d===r?a(i[2],au):au;if(e(m[93],b,h,D))return 0;var
t=j(av),E=k===t?av[1]:d===t?a(i[2],av):av;if(e(m[93],b,h,E))return 1;throw P;default:throw P}}function
jv(b,a){return eT(b,a)}function
jw(b){function
c(e,d){var
c=bP(b,d);if(typeof
c!=="number"&&1===c[0])return w9;return[6,e,a(s[12][15],c)]}function
d(a){return bP(b,a)}return function(a,e){return eS(b,d,c,jr,a,e)}}function
jx(d){function
b(e,f){var
b=bP(d,f);if(typeof
b!=="number"&&1===b[0]){if(0===e[0])return[0,c(s[85],e[1],b)];a(h[30],w_);a(h[51],h[27]);throw P}return[6,e,a(s[12][15],b)]}function
e(a){return dR(d,a)}return function(a,c){return eS(d,e,b,js,a,c)}}function
jy(b){function
c(d,c){var
e=gf(b,c);return[6,d,a(s[7][1],e)]}function
d(a){return jv(b,a)}return function(a,e){return eS(b,d,c,jt,a,e)}}function
eU(o,i,n,l,g){var
b=g[2],d=c(m[3],b,l);if(9===d[0]){var
f=c(o,g,[0,d[1],d[2]]),p=f[3],q=f[1],j=e(i,b,n,f[2]),r=j[1],k=e(i,b,j[2],p);return[0,[0,r,q,k[1]],k[2]]}return a(h[2],w$)}function
xa(a,b,c){return eU(jm,jw,a,b,c)}function
xb(a,b,c){return eU(jo,jx,a,b,c)}function
xc(a,b,c){return eU(jn,jy,a,b,c)}function
bx(a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return 2;case
1:return[0,a[1]];case
2:var
b=a[1],c=bx(a[2]);return[1,bx(b),c];case
3:var
d=a[1],e=bx(a[2]);return[2,bx(d),e];case
4:return[3,bx(a[1])];default:var
f=a[1],g=bx(a[3]);return[4,bx(f),g]}}function
jz(b,a){return[2,b,a]}function
jA(b,a){return[3,b,a]}function
jB(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
jC(b,a){return[5,b,0,a]}function
dT(e,d,b,a){if(typeof
b!=="number"&&0===b[0])if(typeof
a!=="number"&&0===a[0])return[0,d];return c(e,b,a)}function
xd(o,p,h,g,f){var
n=o[2];function
L(f,d,c){try{var
b=e(p,f,c,o),g=b[2],h=b[1],i=[0,[1,h,d,c],g,a(l[32][2],d)];return i}catch(b){b=w(b);if(a(br[20],b))return[0,[0,c],f,d];throw b}}function
b(h,g,f){var
l=c(m[3],n,f);switch(l[0]){case
6:var
E=l[3],Q=l[2];if(e(m[kL][13],n,1,E)){var
r=b(h,g,Q),R=r[1],s=b(r[2],r[3],E),S=s[3],T=s[2];return[0,dT(jC,f,R,s[1]),T,S]}break;case
9:var
p=l[2],q=l[1],F=p.length-1;if(!(3<=F))switch(F){case
0:break;case
1:var
H=j(cZ),U=p[1],V=k===H?cZ[1]:d===H?a(i[2],cZ):cZ;if(e(m[93],n,q,V)){var
t=b(h,g,U);return[0,[4,t[1]],t[2],t[3]]}break;default:var
u=p[1],v=p[2],I=j(a8),W=k===I?a8[1]:d===I?a(i[2],a8):a8;if(e(m[93],n,q,W)){var
w=b(h,g,u),X=w[1],x=b(w[2],w[3],v),Y=x[3],Z=x[2];return[0,dT(jz,f,X,x[1]),Z,Y]}var
J=j(a9),_=k===J?a9[1]:d===J?a(i[2],a9):a9;if(e(m[93],n,q,_)){var
y=b(h,g,u),$=y[1],z=b(y[2],y[3],v),aa=z[3],ab=z[2];return[0,dT(jA,f,$,z[1]),ab,aa]}var
K=j(c0),ac=k===K?c0[1]:d===K?a(i[2],c0):c0;if(e(m[93],n,q,ac)){var
A=b(h,g,u),ad=A[1],B=b(A[2],A[3],v),ae=B[3],af=B[2];return[0,dT(jB,f,ad,B[1]),af,ae]}}return L(h,g,f)}var
C=j(a_),N=k===C?a_[1]:d===C?a(i[2],a_):a_;if(e(m[93],n,f,N))return[0,0,h,g];var
D=j(as),O=k===D?as[1]:d===D?a(i[2],as):as;if(e(m[93],n,f,O))return[0,1,h,g];var
M=G(xe[3],0,o[1],o[2],f);if(a(xf[8],M))return[0,[0,f],h,g];throw P}return b(h,g,f)}function
xg(c,r,b){function
e(b){if(typeof
b==="number"){if(0===b){var
f=j(dH),s=[0,c],t=k===f?dH[1]:d===f?a(i[2],dH):dH;return a(m[21],[0,t,s])}var
g=j(dI),u=[0,c],v=k===g?dI[1]:d===g?a(i[2],dI):dI;return a(m[21],[0,v,u])}else
switch(b[0]){case
0:var
h=j(dN),w=[0,c,b[1]],x=k===h?dN[1]:d===h?a(i[2],dN):dN;return a(m[21],[0,x,w]);case
1:var
l=j(dM),y=[0,c,a(r,b[1])],z=k===l?dM[1]:d===l?a(i[2],dM):dM;return a(m[21],[0,z,y]);case
2:var
A=b[1],B=e(b[2]),n=j(dJ),C=[0,c,e(A),B],D=k===n?dJ[1]:d===n?a(i[2],dJ):dJ;return a(m[21],[0,D,C]);case
3:var
E=b[1],F=e(b[2]),o=j(dK),G=[0,c,e(E),F],H=k===o?dK[1]:d===o?a(i[2],dK):dK;return a(m[21],[0,H,G]);case
4:var
p=j(dL),I=[0,c,e(b[1])],J=k===p?dL[1]:d===p?a(i[2],dL):dL;return a(m[21],[0,J,I]);default:var
K=b[1],L=e(b[3]),q=j(dO),M=[0,c,e(K),L],N=k===q?dO[1]:d===q?a(i[2],dO):dO;return a(m[21],[0,N,M])}}return e(b)}function
jD(h,a){function
d(j,i){var
b=j,a=i;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return e(bQ[1],b,h,a[1])[1];case
4:var
a=a[1];continue;case
5:var
g=a[3],f=a[1],c=1;break;case
1:var
c=0;break;default:var
g=a[2],f=a[1],c=1}if(c){var
b=d(b,f),a=g;continue}return b}}return d(0,a)}function
jE(b){function
d(e){var
b=e;for(;;)switch(b[0]){case
0:return ar[1];case
1:var
f=a(l[29][2],b[1]);return a(ar[5],f);case
5:case
6:var
b=b[1];continue;default:var
g=b[1],h=d(b[2]),i=d(g);return c(ar[7],i,h)}}function
e(l){var
a=l;for(;;){if(typeof
a==="number")var
b=0;else
switch(a[0]){case
1:var
f=a[1],i=f[1],j=d(f[3]),k=d(i);return c(ar[7],k,j);case
4:var
a=a[1];continue;case
5:var
h=a[3],g=a[1],b=1;break;case
0:var
b=0;break;default:var
h=a[2],g=a[1],b=1}if(b){var
m=e(h),n=e(g);return c(ar[7],n,m)}return ar[1]}}return e(b)}var
xh=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=c(g[17],o,gk),b=j(b$);function
q(b){var
c=a(l[29][3],b);return bv(a(l[30][5],c))}var
r=k===b?b$[1]:d===b?a(i[2],b$):b$,e=j(b_),s=k===e?b_[1]:d===e?a(i[2],b_):b_,f=j(b9),t=k===f?b9[1]:d===f?a(i[2],b9):b9,h=j(b8),u=k===h?b8[1]:d===h?a(i[2],b8):b8,m=j(b7),v=k===m?b7[1]:d===m?a(i[2],b7):b7,n=j(bc),w=k===n?bc[1]:d===n?a(i[2],bc):bc;return[0,w,bv,v,u,t,s,r,q,p]}],xi=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=c(g[17],o,gm),b=j(ce);function
q(b){var
c=a(l[29][3],b);return bv(a(l[30][5],c))}var
r=k===b?ce[1]:d===b?a(i[2],ce):ce,e=j(cd),s=k===e?cd[1]:d===e?a(i[2],cd):cd,f=j(cc),t=k===f?cc[1]:d===f?a(i[2],cc):cc,h=j(cb),u=k===h?cb[1]:d===h?a(i[2],cb):cb,m=j(ca),v=k===m?ca[1]:d===m?a(i[2],ca):ca,n=j(c$),w=k===n?c$[1]:d===n?a(i[2],c$):c$;return[0,w,eQ,v,u,t,s,r,q,p]}];function
jF(o){var
e=j(ag),p=k===e?ag[1]:d===e?a(i[2],ag):ag,f=j(af),q=k===f?af[1]:d===f?a(i[2],af):af,g=j(av),b=k===g?av[1]:d===g?a(i[2],av):av;function
h(c,b){return a(m[21],[0,q,[0,c,b]])}function
l(c,b){return a(m[21],[0,p,[0,c,b]])}var
n=h(b,b);function
c(a){return typeof
a==="number"?b:0===a[0]?h(b,l(n,c(a[1]))):l(n,c(a[1]))}return c(o)}function
xj(e){var
b=a(l[29][3],e);if(0===b){var
c=j(au);return k===c?au[1]:d===c?a(i[2],au):au}return jF(a(l[30][2],b))}function
aU(b){if(typeof
b==="number"){if(0===b){var
c=j(au);return k===c?au[1]:d===c?a(i[2],au):au}var
e=j(av);return k===e?av[1]:d===e?a(i[2],av):av}else
switch(b[0]){case
0:var
f=j(bg),q=[0,eQ(b[1])],r=k===f?bg[1]:d===f?a(i[2],bg):bg;return a(m[21],[0,r,q]);case
1:var
g=j(bf),s=[0,bv(b[1])],t=k===g?bf[1]:d===g?a(i[2],bf):bf;return a(m[21],[0,t,s]);case
2:var
u=b[1],v=aU(b[2]),h=j(af),w=[0,aU(u),v],x=k===h?af[1]:d===h?a(i[2],af):af;return a(m[21],[0,x,w]);case
3:var
y=b[1],z=aU(b[2]),l=j(aH),A=[0,aU(y),z],B=k===l?aH[1]:d===l?a(i[2],aH):aH;return a(m[21],[0,B,A]);case
4:var
C=b[1],D=aU(b[2]),n=j(ag),E=[0,aU(C),D],F=k===n?ag[1]:d===n?a(i[2],ag):ag;return a(m[21],[0,F,E]);case
5:var
o=j(be),G=[0,aU(b[1])],H=k===o?be[1]:d===o?a(i[2],be):be;return a(m[21],[0,H,G]);default:var
p=j(aT),I=[0,aU(b[1])],J=k===p?aT[1]:d===p?a(i[2],aT):aT;return a(m[21],[0,J,I])}}var
xk=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=c(g[17],o,gl),b=j(cf);function
q(b){var
c=a(l[29][3],b);return eP(a(l[30][1],c))}var
r=k===b?cf[1]:d===b?a(i[2],cf):cf,e=j(ag),s=k===e?ag[1]:d===e?a(i[2],ag):ag,f=j(aT),t=k===f?aT[1]:d===f?a(i[2],aT):aT,h=j(aH),u=k===h?aH[1]:d===h?a(i[2],aH):aH,m=j(af),v=k===m?af[1]:d===m?a(i[2],af):af,n=j(bd),w=k===n?bd[1]:d===n?a(i[2],bd):bd;return[0,w,aU,v,u,t,s,r,q,p]}];function
go(h,g,f){var
b=[0,h,g,f];for(;;){var
d=b[1];if(0===d)return b[3];var
c=b[2];if(c){var
e=c[1],i=c[2],b=[0,d-1|0,i,a(m[18],[0,e[1],e[2],b[3]])];continue}throw[0,D,xl]}}function
xm(p,f,b){var
u=jE(b),v=a(ar[21],u);function
x(b,a){return[0,a,b+1|0]}var
q=c(g[18],x,v),r=jD(p,b);function
y(b){var
d=f[1],e=c(n[4],xn,b[2]);return[0,a(H[1][6],e),d]}var
o=c(g[17],y,q);function
z(b,f){var
d=m[14],e=c(n[4],xo,b+1|0);return[0,a(H[1][6],e),d]}var
s=c(g[18],z,r);function
A(b,a){return[0,a[1],b[1]]}var
B=e(g[23],A,q,o);function
t(e,b){function
d(b){switch(b[0]){case
0:return a(f[2],b[1]);case
1:var
h=a(l[29][2],b[1]),i=e+c(g[38],h,q)|0;return a(m[9],i);case
2:var
j=b[1],k=d(b[2]),n=[0,d(j),k];return a(m[21],[0,f[3],n]);case
3:var
o=b[1],p=d(b[2]),r=[0,d(o),p];return a(m[21],[0,f[4],r]);case
4:var
s=b[1],t=d(b[2]),u=[0,d(s),t];return a(m[21],[0,f[6],u]);case
5:var
v=[0,d(b[1])];return a(m[21],[0,f[5],v]);default:var
w=b[1],x=a(f[8],b[2]),y=[0,d(w),x];return a(m[21],[0,f[7],y])}}return d(b)}function
C(b,h,e){try{var
p=[0,c(g[38],b,f[9]),[0,h,e]],q=a(m[21],p);return q}catch(b){b=w(b);if(b===O){var
l=j(aw),n=[0,f[1],h,e],o=k===l?aw[1]:d===l?a(i[2],aw):aw;return a(m[21],[0,o,n])}throw b}}function
h(g,f,b){if(typeof
b==="number"){if(0===b){var
n=j(a_);return k===n?a_[1]:d===n?a(i[2],a_):a_}var
o=j(as);return k===o?as[1]:d===o?a(i[2],as):as}else
switch(b[0]){case
0:var
y=g+e(bQ[2],r,p,b[1])|0;return a(m[9],y);case
1:var
l=b[1],v=l[2],w=l[1],x=t(f,l[3]);return C(v,t(f,w),x);case
2:var
z=b[1],A=h(g,f,b[2]),q=j(a8),B=[0,h(g,f,z),A],D=k===q?a8[1]:d===q?a(i[2],a8):a8;return a(m[21],[0,D,B]);case
3:var
E=b[1],F=h(g,f,b[2]),s=j(a9),G=[0,h(g,f,E),F],H=k===s?a9[1]:d===s?a(i[2],a9):a9;return a(m[21],[0,H,G]);case
4:var
u=j(as),I=b[1],J=k===u?as[1]:d===u?a(i[2],as):as,K=h(g,f,I);return c(m[33],K,J);default:var
L=b[1],M=h(g+1|0,f+1|0,b[3]),N=h(g,f,L);return c(m[33],N,M)}}var
D=a(g[1],o),E=a(g[1],s),F=a7(function(b){var
d=e(bQ[2],r,p,b),f=c(n[4],xp,d),g=a(H[1][6],f);return a(m[10],g)},b),G=a(g[9],B),I=a(g[9],s),J=h(a(g[1],o),0,b);function
K(a){return[0,[0,a[1]],a[2]]}var
L=go(D,c(g[17],K,o),J);function
M(a){return[0,[0,a[1]],a[2]]}return[0,go(E,c(g[17],M,s),L),I,G,F]}var
o=[0,i0,ge,i1,i2,i3,i4,N,am,u,aG,an,aR,aS,a8,a9,cZ,c0,a_,as,c1,c2,tm,c3,c4,tq,c5,c6,c7,tv,tx,tz,a$,ba,bb,bc,c8,c9,c_,c$,bd,tK,at,tN,da,db,dc,dd,de,df,dg,dh,di,au,av,t0,t2,t4,t6,t8,i5,i6,i7,i8,aw,b7,b8,b9,b_,b$,ui,uk,i9,i_,i$,ca,cb,cc,cd,ce,ja,jb,jc,jd,af,aH,aT,ag,uC,be,cf,bf,bg,dj,dk,dl,dm,dn,dp,dq,dr,ds,dt,du,dv,dw,dx,dy,dz,dA,dB,dC,dD,dE,dF,dG,u5,u9,vb,dH,dI,dJ,dK,dL,dM,dN,dO,vD,vH,vL,vP,dP,vW,vX,P,cg,gf,je,eP,ch,ax,dQ,jf,gg,v4,jg,v6,bP,bv,jh,v8,eQ,dR,bh,bi,bj,ji,jj,wf,dQ,ax,gh,gi,gj,eR,ws,wv,ww,jk,jl,wK,wM,dS,gk,gl,gm,gn,jm,jn,jo,wQ,jp,bQ,eS,jr,js,jt,bP,dR,ju,jv,jw,jx,jy,eU,xa,xb,xc,bx,jz,jA,jB,jC,dT,xd,xg,jD,jE,xh,xi,jF,xj,aU,xk,go,xm,function(f,e){var
c=e,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[3],i=d[2],j=a(H[1][6],d[1]),c=G(m[40],j,i,h,c),b=g;continue}return c}}];function
dU(d){var
b=d;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:return[0,a(l[29][1],b[1]),0];case
2:var
b=b[2];continue;case
3:var
e=b[1],f=dU(b[2]),g=dU(e);return c(h[25],g,f);case
4:var
i=b[1],j=dU(b[2]),k=dU(i);return c(h[25],k,j)}return 0}}function
xq(a,f,e){return function(h){var
a=h;for(;;){if(a){var
d=a[1],i=a[2];try{var
j=c(g[7],e,d),k=c(g[7],f,d)===j?1:0,b=k}catch(a){a=w(a);if(a[1]!==jq)throw a;var
b=0}if(b){var
a=i;continue}return b}return 1}}(a)}function
xr(d,b,f){function
e(i,h){var
d=i,b=h;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
j=a(l[29][1],b[1]),k=c(g[7],f,j)[2];return c(H[1][10][7],d,k);case
2:var
b=b[2];continue;case
3:case
4:var
m=b[2],d=e(d,b[1]),b=m;continue}return d}}return e(d,b)}function
jG(e,d,a){if(a){var
g=a[2],b=c(l[6],e,[0,a[1],d]);if(b){var
h=b[1],f=jG(e,d,g);return f?[0,[0,h,f[1]]]:0}return 0}return xs}function
xt(e,d){var
c=e,b=d;for(;;){if(b){var
f=b[2],g=[0,c,[0,a(jH[108],b[1])]],c=a(jH[kT],g),b=f;continue}return c}}var
dV=[d,function(a){return e(o[7],xw,xv,xu)}],dW=[d,function(a){return e(o[7],xz,xy,xx)}],dX=[d,function(a){return e(o[7],xC,xB,xA)}],dY=[d,function(a){return e(o[7],xF,xE,xD)}];function
dZ(c,b){if(typeof
b==="number"){var
e=j(dX),h=[0,c],l=k===e?dX[1]:d===e?a(i[2],dX):dX;return a(m[21],[0,l,h])}else{if(0===b[0]){var
f=j(dW),n=[0,c,b[1]],o=k===f?dW[1]:d===f?a(i[2],dW):dW;return a(m[21],[0,o,n])}var
p=b[2],q=b[1],r=dZ(c,b[3]),g=j(dV),s=[0,c,dZ(c,q),p,r],t=k===g?dV[1]:d===g?a(i[2],dV):dV;return a(m[21],[0,t,s])}}function
gp(b){if(b){var
c=b[1][1],d=0,f=function(d,b){var
e=b[1],f=a(l[30][2],b[2]);return G(s[88],c,f,e,d)};return e(g[20],f,d,b)}return 0}function
gq(b,a){return typeof
a==="number"?c(h[54],b,xG):0===a[0]?G(n[1],b,xH,o[a1],a[1]):bS(n[1],b,xI,gq,a[1],o[a1],a[2],gq,a[3])}function
eV(b){if(typeof
b==="number"){var
c=o[56],n=j(c);return k===n?c[1]:d===n?a(i[2],c):c}else
switch(b[0]){case
0:var
t=b[1],u=eV(b[2]),v=[0,e(o[bT],o[35],o[d9],t),u],f=o[57],p=j(f),w=k===p?f[1]:d===p?a(i[2],f):f;return a(m[21],[0,w,v]);case
1:var
x=b[1],y=eV(b[2]),z=[0,e(o[bT],o[35],o[d9],x),y],g=o[58],q=j(g),A=k===q?g[1]:d===q?a(i[2],g):g;return a(m[21],[0,A,z]);default:var
h=o[55],r=j(h),B=b[3],C=b[2],D=b[1],E=k===r?h[1]:d===r?a(i[2],h):h,F=e(o[k6],E,eV,B),G=e(o[bT],o[35],o[d9],C),H=[0,e(o[bT],o[35],o[d9],D),G,F],l=o[59],s=j(l),I=k===s?l[1]:d===s?a(i[2],l):l;return a(m[21],[0,I,H])}}function
by(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:return 1;case
1:return 1;case
2:return 1+by(a[2])|0;case
5:return 1;default:var
b=a[1],c=by(a[2]);return by(b)+c|0}}function
eW(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:var
b=a[2],c=by(a[1]);return eW(b)+c|0;case
1:var
d=a[2],f=by(a[1]);return eW(d)+f|0;default:var
h=a[3],i=a[2],j=a[1],k=0,l=function(b,a){return eW(a)+b|0},m=e(g[20],l,k,h),n=by(i);return(by(j)+n|0)+m|0}}function
jI(a){return eV(a)}function
aI(b,a){return C(n[1],b,xJ,o[a1],a[1],o[141],a[2])}function
bR(d,b){if(typeof
b==="number")return c(n[1],d,xK);else
switch(b[0]){case
0:var
f=b[2],g=b[1],h=a(o[bm],o[a1]);return C(n[1],d,xL,h,g,bR,f);case
1:var
i=b[2],j=b[1],k=a(o[bm],o[a1]);return C(n[1],d,xM,k,j,bR,i);default:var
l=b[3],m=b[2],p=b[1],q=e(o[158],xO,xN,bR),r=a(o[bm],o[a1]),s=a(o[bm],o[a1]);return bS(n[1],d,xP,s,p,r,m,q,l)}}function
gr(h,g,f,e,b){if(b){var
i=b[1],m=i[2],n=i[1],c=gr(h,g,f,e,b[2]),j=c[3],k=c[2],l=c[1];try{var
d=aX(o[k0],h,g,k,j,m),p=[0,[0,[0,n,d[1]],l],d[2],d[3]];return p}catch(b){b=w(b);if(a(br[20],b))return[0,l,k,j];throw b}}return[0,0,f,e]}function
gs(d,c,h,g,f){var
i=a(l[32][1],0),b=aX(o[k0],d,c,h,i,f),j=b[1],e=gr(d,c,b[2],b[3],g);return[0,e[1],j,e[2]]}var
d0=[d,function(q){var
b=o[55],f=j(b),l=k===f?b[1]:d===f?a(i[2],b):b,c=o[35],g=j(c),m=o[d9],n=k===g?c[1]:d===g?a(i[2],c):c,e=o[35],h=j(e),p=k===h?e[1]:d===h?a(i[2],e):e;return[0,p,n,m,l,jI]}],d1=[d,function(s){var
m=c(o[bT],o[39],o[d6]),b=o[fb],g=j(b),n=k===g?b[1]:d===g?a(i[2],b):b,e=o[39],h=j(e),p=o[d6],q=k===h?e[1]:d===h?a(i[2],e):e,f=o[39],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,n,m]}],xQ=[d,function(s){var
m=c(o[bT],o[39],o[d6]),b=o[fb],g=j(b),n=k===g?b[1]:d===g?a(i[2],b):b,e=o[43],h=j(e),p=o[k2],q=k===h?e[1]:d===h?a(i[2],e):e,f=o[40],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,n,m]}];function
jJ(d,b,a){return c(d,b,a)?1:c(d,a,b)?0:1}function
jK(d,b,a){function
e(b){return jJ(d,a,b)}return c(g[27],e,b)}function
jL(i,g){var
d=0,b=g;for(;;){if(b){var
f=b[2],e=b[1];if(a(i,e))return[0,[0,e],c(h[25],d,f)];var
d=[0,e,d],b=f;continue}return[0,0,d]}}function
jM(b,a){return jL(function(c){return jK(b,a,c)},a)}function
gt(a,d){var
b=jM(a,d),c=b[1];if(c){var
e=c[1];return[0,e,gt(a,b[2])]}return 0}function
xR(b){return gt(function(c,b){var
d=a(m[8],b),f=a(m[8],c);return e(xT[37],xS[16],f,d)},b)}function
jN(b,q,p,n,l){var
f=o[km],g=j(f),r=[0,b[2]],s=k===g?f[1]:d===g?a(i[2],f):f,h=a(m[21],[0,s,r]),t=c(o[li],b[2],b[3]),u=e(o[lr],h,t,l),v=gp(n),w=dZ(b[1],v);function
x(l){var
r=a(aV[48][6],l),f=j(dY),n=0,s=[0,[0,xU,q,p],0],t=[0,b[1]],v=k===f?dY[1]:d===f?a(i[2],dY):dY,x=[0,[0,xV,w,a(m[21],[0,v,t])],s],e=o[kv],g=j(e),y=[0,h],z=k===g?e[1]:d===g?a(i[2],e):e,A=[0,[0,xW,u,a(m[21],[0,z,y])],x],B=c(o[kl],A,r),C=[0,a(ao[52],B),n];return a(K[66][20],C)}return a(d2[63][8],x)}function
jO(d,b){function
e(b){var
c=b[1];return[0,function(d){var
e=[0,a(b[2],0),d],c=a(b[3],e);return c?[0,[0,c[1],b]]:0},c]}var
f=c(g[17],e,d);function
h(a){return a[1]}var
i=c(g[17],h,b);return c(l[6],f,i)}function
eX(e,a){function
b(a){if(a){var
f=a[2],c=jO(e,a[1]);if(c){var
g=c[1],d=b(f);return d?[0,[0,g,d[1]]]:0}return 0}return xX}return b(a)}function
xY(d,a,b){c(h[54],a,xZ);function
e(b){return G(n[1],a,x0,d,b)}c(g[15],e,b);return c(h[54],a,x1)}function
jP(f,d,b){function
i(k,b,j){var
d=b[2],m=b[1];function
n(b,a){return[0,b[1],a]}var
e=c(l[23],n,j);function
o(b){try{var
f=c(g[7],k,b)[1],d=f}catch(b){b=w(b);if(b[1]!==fe)throw b;var
d=a(h[2],x2)}return c(g[38],d,e)}try{var
t=c(d[5],m,o),i=t}catch(b){b=w(b);if(!a(br[20],b))throw b;var
p=function(a){return a[1]},q=c(g[17],p,e),r=[0,a(d[2],0),q],f=a(d[3],r),s=f?f[1]:a(h[2],x3),i=s}return i}var
j=c(g[47],f,d);function
k(b){function
f(c){var
d=c[2],g=c[1],f=iZ(a(d[2][4],d[1]),g);return e(l[11],X,f,b)}var
d=c(g[33],f,j);return i(d[1],d[2],b)}return c(g[17],k,b)}function
jQ(B,b){function
e(b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
v=b[3],w=b[2],C=b[1];return c(l[33][3],w,B)?[1,C,w,v]:[0,v];case
2:var
D=b[2],g=e(b[1]),h=e(D);if(typeof
g!=="number"&&0===g[0])if(typeof
h!=="number"&&0===h[0]){var
n=o[14],x=j(n),E=[0,g[1],h[1]],F=k===x?n[1]:d===x?a(i[2],n):n;return[0,a(m[21],[0,F,E])]}return[2,g,h];case
3:var
G=b[2],p=e(b[1]),q=e(G);if(typeof
p!=="number"&&0===p[0])if(typeof
q!=="number"&&0===q[0]){var
r=o[15],y=j(r),H=[0,p[1],q[1]],I=k===y?r[1]:d===y?a(i[2],r):r;return[0,a(m[21],[0,I,H])]}return[3,p,q];case
4:var
s=e(b[1]);if(typeof
s!=="number"&&0===s[0]){var
t=o[16],z=j(t),J=[0,s[1]],K=k===z?t[1]:d===z?a(i[2],t):t;return[0,a(m[21],[0,K,J])]}return[4,s];default:var
A=b[2],L=b[3],u=e(b[1]),f=e(L);if(typeof
u!=="number"&&0===u[0]){var
M=u[1];if(A)return f;if(typeof
f!=="number"&&0===f[0])return[0,c(m[33],M,f[1])]}return[5,u,A,f]}}return e(b)}function
bk(c,b){if(typeof
c==="number"){if(0===c){if(typeof
b==="number")if(0===b)return 0}else
if(typeof
b==="number")if(0!==b)return 1}else
switch(c[0]){case
0:return[0,c[1]];case
1:if(typeof
b!=="number"&&1===b[0])return b;break;case
2:if(typeof
b!=="number"&&2===b[0]){var
d=b[1],e=c[1],f=bk(c[2],b[2]);return[2,bk(e,d),f]}break;case
3:if(typeof
b!=="number"&&3===b[0]){var
g=b[1],i=c[1],j=bk(c[2],b[2]);return[3,bk(i,g),j]}break;case
4:if(typeof
b!=="number"&&4===b[0])return[4,bk(c[1],b[1])];break;default:if(typeof
b!=="number"&&5===b[0]){var
k=b[2],l=b[1],m=c[1],n=bk(c[3],b[3]);return[5,bk(m,l),k,n]}}return a(h[2],x4)}var
eY=[a0,x5,aZ(0)];function
gu(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return e(g[21],d,b,c)}function
gv(k,j,i,h,f,t,D,s,r,C){var
m=gu(s,r)[1],n=eO(k,j,i,h,m),b=n[1],u=n[2],p=eX(t,b);if(p){var
q=p[1],v=c(g[47],b,q),w=l[33][1],x=function(b,a){return c(l[33][4],a,b)},y=e(g[20],x,w,u),z=function(f,b){var
d=b[2],h=b[1],i=l[33][1],j=a(d[2][4],d[1]);function
k(b,a){var
d=c(g[7],h,b)[2];return c(l[33][4],d,a)}var
m=e(ar[15],k,j,i);return c(l[33][7],f,m)},d=jQ(e(g[20],z,y,v),m),A=jP(b,q,eO(k,j,i,h,d)[1]),B=gd(d);return[0,[0,B,d,e(o[k6],f[4],f[5],A)]]}return 0}function
gw(d,c,b){var
f=a(d2[63][5],b);return e(ao[13],d,c,f)}function
bl(aH,aG,aF,aE,aD,p,n){return function(aI,aJ){function
b(b){var
q=a(aV[48][4],b),J=a(aV[48][6],b),L=a(aV[48][13],b);try{var
v=[0,a(aV[48][5],b),q],r=gs(v,aH,o[e7][3],L,J),T=r[2],U=r[1],x=a(o[e7][4],r[3]),y=j(p),s=k===y?p[1]:d===y?a(i[2],p):p,z=j(n),V=k===z?n[1]:d===z?a(i[2],n):n,A=gv(aG,aF,aE,aD,s,aI,x,U,T,v);if(A)var
t=A[1],C=t[2],W=t[3],X=t[1],f=e(o[j$],q,V,C),u=f[3],Y=f[4],Z=f[2],_=f[1],D=function(a){return c(ao[2],0,a[1])},$=c(g[17],D,u),aa=a(K[66][20],$),ab=c(g[17],D,Z),ac=a(K[66][20],ab),ad=function(a){return[0,c(Q[10],0,[1,[0,a]])]},E=gw(0,a(H[1][6],ya),b),ae=function(b){var
c=b[2];return[0,a(m[10],b[1]),c]},af=c(g[17],ae,u),l=o[22],F=j(l),ag=0,ah=[0,s[4]],ai=k===F?l[1]:d===F?a(i[2],l):l,aj=[0,ac,[0,aa,[0,jN(s,W,a(m[21],[0,ai,ah]),af,Y),ag]]],ak=a(K[66][20],aj),al=c(o[kw],q,C),am=a(g[9],al),an=function(a){return c(g[7],x,a[2]-1|0)},ap=c(g[17],an,u),aq=c(h[25],am,ap),ar=c(K[66][3],ak,aJ),as=a(ao[77],0),at=c(K[66][3],as,ar),au=[0,a(m[10],E),aq],av=a(m[34],au),aw=[0,a(ao[45],av),0],ax=c(g[17],m[10],X),ay=[0,a(ao[j9],ax),aw],az=[0,at,[0,a(K[66][20],ay),0]],aA=ad(E),aB=G(ao[ln],1,yb,aA,_),I=c(K[66][19],aB,az);else
var
aC=a(bw[3],yc),I=c(K[66][4],0,aC);return I}catch(b){b=w(b);if(b===o[ke]){var
M=a(bw[3],x6);return c(K[66][4],0,M)}if(b===B[23]){var
N=a(bw[3],x7);return c(K[66][4],0,N)}if(b===eY){a(h[51],h[27]);var
O=c(h[16],x9,x8),P=c(h[16],x_,O),R=c(h[16],x$,P),S=a(bw[3],R);return c(K[66][4],0,S)}throw b}}return a(d2[63][8],b)}}function
jR(y,x,w){var
b=o[43],n=j(b),p=k===n?b[1]:d===n?a(i[2],b):b,f=o[40],q=j(f),z=o[k2],r=k===q?f[1]:d===q?a(i[2],f):f,g=o[fb],s=j(g),A=k===s?g[1]:d===s?a(i[2],g):g,h=o[22],t=j(h),B=[0,A],C=k===t?h[1]:d===t?a(i[2],h):h,D=a(m[21],[0,C,B]),l=o[km],u=j(l),E=[0,p],F=k===u?l[1]:d===u?a(i[2],l):l,v=a(m[21],[0,F,E]),G=c(o[li],p,z),H=e(o[lr],v,G,w),I=dZ(r,gp(x));function
J(g){var
l=a(aV[48][6],g),n=[0,e(o[7],yg,yf,ye),[0,r]],p=[0,[0,yh,I,a(m[21],n)],[0,[0,yd,y,D],0]],b=o[kv],f=j(b),h=0,q=[0,v],s=k===f?b[1]:d===f?a(i[2],b):b,t=[0,[0,yi,H,a(m[21],[0,s,q])],p],u=c(o[kl],t,l),w=[0,a(ao[52],u),h];return a(K[66][20],w)}return a(d2[63][8],J)}function
d3(aJ){return function(aK){var
L=o[197],M=s[e9],N=s[kT],O=s[121],P=s[122],f=[d,function(s){var
m=c(o[bT],o[39],o[d6]),b=o[fb],g=j(b),n=k===g?b[1]:d===g?a(i[2],b):b,e=o[43],h=j(e),p=o[d6],q=k===h?e[1]:d===h?a(i[2],e):e,f=o[40],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,n,m]}];function
b(b){var
p=a(aV[48][4],b),R=a(aV[48][6],b),S=a(aV[48][13],b);try{var
u=[0,a(aV[48][5],b),p],q=gs(u,L,o[e7][3],S,R),v=q[2],x=q[1],y=a(o[e7][4],q[3]),z=j(f),Z=k===z?f[1]:d===z?a(i[2],f):f,_=function(b){var
c=b[2],d=b[1];return[0,d,aQ(a(s[73],s[gQ]),c)]},$=c(g[17],_,x),A=gv(M,N,O,P,Z,aJ,y,$,aQ(a(s[73],s[gQ]),v),u);if(A)var
r=A[1],aa=r[3],ab=r[2],ac=r[1],ad=function(a){return c(g[31],a[1],ac)},C=gu(c(g[35],ad,x),v),ae=C[2],D=bk(ab,C[1]),l=o[213],E=j(l),af=k===E?l[1]:d===E?a(i[2],l):l,n=e(o[j$],p,af,D),t=n[3],ag=n[4],ah=n[2],ai=n[1],F=function(a){return c(ao[2],0,a[1])},aj=c(g[17],F,t),ak=a(K[66][20],aj),al=c(g[17],F,ah),am=a(K[66][20],al),an=function(a){return[0,c(Q[10],0,[1,[0,a]])]},I=gw(0,a(H[1][6],yp),b),ap=function(b){var
c=b[2];return[0,a(m[10],b[1]),c]},aq=[0,am,[0,ak,[0,jR(aa,c(g[17],ap,t),ag),0]]],ar=a(K[66][20],aq),as=c(o[kw],p,D),at=a(g[9],as),au=function(a){return c(g[7],y,a[2]-1|0)},av=c(g[17],au,t),aw=c(h[25],at,av),ax=c(K[66][3],ar,aK),ay=a(ao[77],0),az=c(K[66][3],ay,ax),aA=[0,a(m[10],I),aw],aB=a(m[34],aA),aC=[0,a(ao[45],aB),0],aD=c(g[17],m[10],ae),aE=[0,a(ao[j9],aD),aC],aF=[0,az,[0,a(K[66][20],aE),0]],aG=an(I),aH=G(ao[ln],1,yq,aG,ai),J=c(K[66][19],aH,aF);else
var
aI=a(bw[3],yr),J=c(K[66][4],0,aI);return J}catch(b){b=w(b);if(b===o[ke]){var
T=a(bw[3],yj);return c(K[66][4],0,T)}if(b===B[23]){var
U=a(bw[3],yk);return c(K[66][4],0,U)}if(b===eY){a(h[51],h[27]);var
V=c(h[16],ym,yl),W=c(h[16],yn,V),X=c(h[16],yo,W),Y=a(bw[3],X);return c(K[66][4],0,Y)}throw b}}return a(d2[63][8],b)}}function
jS(d,c){var
b=a(d,c);return b?[0,[0,b[1],0]]:0}var
jT=a(cW[1],[0,X,bn[21]]),gx=a(yt[8],ys)?0:[d,function(a){throw eY}];function
jV(o,n){var
f=j(gx);if(k!==f)if(d===f)a(i[2],gx);var
p=[0,yx,[0,yw,[0,c(h[16],yv,yu[36]),0]]],q=a(yy[3],0),m=e(g[20],yz[4],q,p),b=e(l[35],m,[0,m],[0,o,n]);return 0===b[0]?b[1]:a(h[2],b[1])}function
yA(a){return jV(a[1],a[2])}var
jW=c(jT[6],jU,yA);function
gy(c,b){return a(jW,[0,c,b])}function
d4(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,d4(a[2])];default:var
c=a[2],d=a[1],e=d4(a[3]);return[2,d4(d),c,e]}}function
gz(f,b){var
d=gy(f,b);if(d){var
e=a(aF[51],d[1]);return c(s[111],b,e)?[0,e]:(a(h[30],yB),0)}return 0}function
jX(f,b){function
i(a){var
b=a[2];return[0,d4(a[1]),b]}var
d=gy(f,c(g[17],i,b));if(d){var
e=a(aF[54],d[1]);return c(s[89],b,e)?[0,e]:(a(h[30],yC),a(h[51],h[27]),0)}return 0}function
ci(e,d,b){function
f(i,h){var
b=i,d=h;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
g=a(l[29][1],b[1]);return e<=g?c(ar[4],g-e|0,d):d;case
2:var
b=b[2];continue;case
3:case
4:var
j=b[1],k=f(b[2],d),b=j,d=k;continue}return d}}return f(b,d)}function
cj(a){return ci(0,ar[1],a)}function
aW(b,d){function
c(b){if(typeof
b!=="number")switch(b[0]){case
0:var
e=a(d,a(l[29][1],b[1]));return[0,a(l[30][1],e)];case
2:var
f=b[1];return[2,f,c(b[2])];case
3:var
g=b[1],h=c(b[2]);return[3,c(g),h];case
4:var
i=b[1],j=c(b[2]);return[4,c(i),j]}return b}return c(b)}function
eZ(a){function
d(i,h,f){var
b=i,a=h,c=f;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=ci(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=ci(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=ci(b,ci(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return e(g[20],q,p,n)}}return d(0,a,ar[1])}function
e0(b,f){function
d(c,b){return b<c?b:a(f,b-c|0)+c|0}function
e(b,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
f=a[1],g=e(b+1|0,a[2]);return[0,aW(f,function(a){return d(b,a)}),g];case
1:var
h=a[1],i=e(b+1|0,a[2]);return[1,aW(h,function(a){return d(b,a)}),i];default:var
j=a[3],k=a[2],l=a[1],m=function(a){return e(b+1|0,a)},n=c(s[10],m,j),o=aW(k,function(a){return d(b,a)});return[2,aW(l,function(a){return d(b,a)}),o,n]}}return e(0,b)}function
ck(d,b){function
e(b){var
c=b[2];return[0,a(s[71],b[1]),c]}return a(d,c(g[17],e,b))}var
gA=a(cW[1],[0,X,bn[21]]),jY=a(cW[1],[0,X,bn[21]]);function
yD(a){var
b=a[1],d=a[2];return ck(c(aF[91],b[1],b[2]),d)}var
jZ=c(gA[6],yE,yD);function
yF(a){var
b=a[1],d=a[2];return ck(c(aF[92],b[1],b[2]),d)}var
j0=c(gA[6],yG,yF);function
yH(b){var
c=b[2];return ck(a(aF[30],b[1]),c)}var
j1=c(jY[6],yI,yH);function
yJ(b,a){return e(o[bA],aI,b,a[1])}var
yK=a(o[bm],aI),j2=[0,yL,eM,function(a){var
b=a[2];return ck(c(aF[29],a[1],aF[5]),b)},cj,aW,yK,yJ];function
yM(b,a){return e(o[bA],aI,b,a[1])}var
yN=a(o[bm],aI),j3=[0,yO,eM,function(a){var
b=a[2];return ck(c(aF[29],a[1],aF[5]),b)},cj,aW,yN,yM];function
yP(b,a){return e(o[bA],aI,b,a[1])}var
gB=[0,yQ,eM,j1,cj,aW,a(o[bm],aI),yP];function
gC(c,b){function
d(b,a){return e(o[bA],aI,b,a[1])}var
f=a(o[bm],aI);function
g(a){return gz(a[1],a[2])}return[0,yR,function(a){return[0,c,b]},g,cj,aW,f,d]}function
gD(c,b){function
d(b,a){return e(o[bA],aI,b,a[1])}var
f=a(o[bm],aI);function
g(a){return gz(a[1],a[2])}return[0,yS,function(a){return[0,c,b]},g,cj,aW,f,d]}function
gE(b,a){function
c(b,a){return e(o[bA],o[a1],b,a[1])}function
d(a){var
b=a[2],c=a[1];return jS(function(a){return jX(c,a)},b)}return[0,yT,function(c){return[0,b,a]},d,eZ,e0,bR,c]}var
gF=[0,yU,gb,jZ,eZ,e0,bR,function(b,a){return e(o[bA],o[a1],b,a[1])}],j4=[0,yV,gb,j0,eZ,e0,bR,function(b,a){return e(o[bA],o[a1],b,a[1])}];function
yW(b){var
a=eX([0,gF,0],eO(s[96],s[94],s[97],s[98],b)[1]);if(a){var
d=a[1],e=function(a){return a[1]};return[0,c(g[17],e,d)]}return 0}var
yX=a(bl(o[e2],s[e$],s[e8],s[e_],s[fd],d1,o[e6]),[0,j2,0]);function
yY(b){var
c=[0,gC(yZ,[0,b]),0];return a(bl(o[e2],s[e$],s[e8],s[e_],s[fd],d1,o[e6]),c)}var
y0=d3([0,j3,0]);function
y1(a){return d3([0,gD(y2,[0,a]),0])}function
y3(b){var
c=[0,gE(y4,[0,b]),0];return a(bl(o[fa],s[96],s[94],s[97],s[98],d0,o[e4]),c)}var
y6=[0,gE(y5,0),0],y7=a(bl(o[fa],s[96],s[94],s[97],s[98],d0,o[e4]),y6),y9=[0,gC(y8,0),0],y_=a(bl(o[e2],s[e$],s[e8],s[e_],s[fd],d1,o[e6]),y9),za=d3([0,gD(y$,0),0]),zb=a(bl(o[fa],s[96],s[94],s[97],s[98],d0,o[e4]),[0,gF,0]),zc=a(bl(o[fa],s[96],s[94],s[97],s[98],d0,o[e4]),[0,j4,0]),zd=d3([0,gB,0]),$=[0,sG,sH,eK,f$,eL,ga,gb,eM,bu,aQ,a7,gd,eN,cX,s1,eO,ar,s3,iZ,o,dU,xq,xr,jG,xt,dV,dW,dX,dY,dZ,gp,gq,by,eW,jI,aI,bR,gr,gs,d0,d1,xQ,jJ,jK,jL,jM,gt,xR,jN,jO,eX,eX,xY,jP,jQ,bk,eY,gu,gv,gw,bl,jR,d3,jS,jT,jU,gx,jV,jW,gy,d4,gz,jX,ci,cj,aW,eZ,e0,ck,gA,jY,jZ,j0,j1,j2,j3,gB,gC,gD,gE,gF,j4,yW,yX,yY,y0,y1,y3,y7,y_,za,zb,zc,zd,a(bl(o[e2],s[e$],s[e8],s[e_],s[fd],d1,o[e6]),[0,gB,0])];aY(774,$,"Micromega_plugin.Coq_micromega");a(ap[12],x);function
ze(e){var
b=[28,[0,0,[31,c(Q[10],0,[0,[0,[0,x,zf],0],0])]]],d=a(H[1][6],zg);return G(r[6][4],1,0,d,b)}var
zh=[0,function(b,a){return ao[54]}];e(r[6][9],0,[0,x,zi],zh);c(ap[19],ze,x);var
zj=0,zl=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(a){var
b=c(r[12][23],a,f);return c($[98],-1,b)}}return a(h[2],zk)},zj],zn=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],g=a(W[6],cl[7]),i=c(r[12][2][7],g,f),j=a(W[6],r[1][1]),k=c(r[12][2][7],j,e);return function(a){var
b=c(r[12][23],a,k);return c($[98],i,b)}}}return a(h[2],zm)},zl],zo=a(ay[12],zn);e(r[6][9],0,[0,x,zp],zo);function
zq(p){var
i=[0,a(H[1][7],zr)],b=r[1][1],g=0,h=0;if(0===b[0]){var
j=[0,[0,zt,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],i])],h]],g],l=[0,a(H[1][7],zu)],d=r[1][1],k=0;if(0===d[0]){var
m=[0,[1,c(Q[10],0,[0,[5,[0,d[1]]],l])],k],n=[0,a(H[1][7],zw)],f=cl[7];if(0===f[0]){var
o=[0,[0,zy,[0,[1,c(Q[10],0,[0,[5,[0,f[1]]],n])],m]],j];return e(r[9][4],[0,x,zz],0,o)}throw[0,D,zx]}throw[0,D,zv]}throw[0,D,zs]}c(ap[19],zq,x);var
zA=0,zC=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[102],d)}}return a(h[2],zB)},zA],zD=a(ay[12],zC);e(r[6][9],0,[0,x,zE],zD);function
zF(i){var
g=[0,a(H[1][7],zG)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,zI,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,zJ],0,h)}throw[0,D,zH]}c(ap[19],zF,x);var
zK=0,zM=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[103],d)}}return a(h[2],zL)},zK],zN=a(ay[12],zM);e(r[6][9],0,[0,x,zO],zN);function
zP(i){var
g=[0,a(H[1][7],zQ)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,zS,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,zT],0,h)}throw[0,D,zR]}c(ap[19],zP,x);var
zU=0,zW=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[104],d)}}return a(h[2],zV)},zU],zX=a(ay[12],zW);e(r[6][9],0,[0,x,zY],zX);function
zZ(i){var
g=[0,a(H[1][7],z0)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,z2,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,z3],0,h)}throw[0,D,z1]}c(ap[19],zZ,x);var
z4=0,z6=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[kL],d)}}return a(h[2],z5)},z4],z7=a(ay[12],z6);e(r[6][9],0,[0,x,z8],z7);function
z9(i){var
g=[0,a(H[1][7],z_)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,Aa,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,Ab],0,h)}throw[0,D,z$]}c(ap[19],z9,x);var
Ac=0,Ae=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[99],d)}}return a(h[2],Ad)},Ac],Af=a(ay[12],Ae);e(r[6][9],0,[0,x,Ag],Af);function
Ah(i){var
g=[0,a(H[1][7],Ai)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,Ak,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,Al],0,h)}throw[0,D,Aj]}c(ap[19],Ah,x);var
Am=0,Ao=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[gN],d)}}return a(h[2],An)},Am],Ap=a(ay[12],Ao);e(r[6][9],0,[0,x,Aq],Ap);function
Ar(i){var
g=[0,a(H[1][7],As)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,Au,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,Av],0,h)}throw[0,D,At]}c(ap[19],Ar,x);var
Aw=0,Ay=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[101],d)}}return a(h[2],Ax)},Aw],Az=a(ay[12],Ay);e(r[6][9],0,[0,x,AA],Az);function
AB(i){var
g=[0,a(H[1][7],AC)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,AE,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,AF],0,h)}throw[0,D,AD]}c(ap[19],AB,x);var
AG=0,AI=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[94],d)}}return a(h[2],AH)},AG],AJ=a(ay[12],AI);e(r[6][9],0,[0,x,AK],AJ);function
AL(i){var
g=[0,a(H[1][7],AM)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,AO,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,AP],0,h)}throw[0,D,AN]}c(ap[19],AL,x);var
AQ=0,AS=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(b){var
d=c(r[12][23],b,f);return a($[96],d)}}return a(h[2],AR)},AQ],AT=a(ay[12],AS);e(r[6][9],0,[0,x,AU],AT);function
AV(i){var
g=[0,a(H[1][7],AW)],b=r[1][1],d=0,f=0;if(0===b[0]){var
h=[0,[0,AY,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],g])],f]],d];return e(r[9][4],[0,x,AZ],0,h)}throw[0,D,AX]}c(ap[19],AV,x);var
A0=0,A2=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(a){var
b=c(r[12][23],a,f);return c($[97],-1,b)}}return a(h[2],A1)},A0],A4=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],g=a(W[6],cl[7]),i=c(r[12][2][7],g,f),j=a(W[6],r[1][1]),k=c(r[12][2][7],j,e);return function(a){var
b=c(r[12][23],a,k);return c($[97],i,b)}}}return a(h[2],A3)},A2],A5=a(ay[12],A4);e(r[6][9],0,[0,x,A6],A5);function
A7(p){var
i=[0,a(H[1][7],A8)],b=r[1][1],g=0,h=0;if(0===b[0]){var
j=[0,[0,A_,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],i])],h]],g],l=[0,a(H[1][7],A$)],d=r[1][1],k=0;if(0===d[0]){var
m=[0,[1,c(Q[10],0,[0,[5,[0,d[1]]],l])],k],n=[0,a(H[1][7],Bb)],f=cl[7];if(0===f[0]){var
o=[0,[0,Bd,[0,[1,c(Q[10],0,[0,[5,[0,f[1]]],n])],m]],j];return e(r[9][4],[0,x,Be],0,o)}throw[0,D,Bc]}throw[0,D,Ba]}throw[0,D,A9]}c(ap[19],A7,x);var
Bf=0,Bh=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(W[6],r[1][1]),f=c(r[12][2][7],e,d);return function(a){var
b=c(r[12][23],a,f);return c($[95],-1,b)}}return a(h[2],Bg)},Bf],Bj=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],g=a(W[6],cl[7]),i=c(r[12][2][7],g,f),j=a(W[6],r[1][1]),k=c(r[12][2][7],j,e);return function(a){var
b=c(r[12][23],a,k);return c($[95],i,b)}}}return a(h[2],Bi)},Bh],Bk=a(ay[12],Bj);e(r[6][9],0,[0,x,Bl],Bk);function
Bm(p){var
i=[0,a(H[1][7],Bn)],b=r[1][1],g=0,h=0;if(0===b[0]){var
j=[0,[0,Bp,[0,[1,c(Q[10],0,[0,[5,[0,b[1]]],i])],h]],g],l=[0,a(H[1][7],Bq)],d=r[1][1],k=0;if(0===d[0]){var
m=[0,[1,c(Q[10],0,[0,[5,[0,d[1]]],l])],k],n=[0,a(H[1][7],Bs)],f=cl[7];if(0===f[0]){var
o=[0,[0,Bu,[0,[1,c(Q[10],0,[0,[5,[0,f[1]]],n])],m]],j];return e(r[9][4],[0,x,Bv],0,o)}throw[0,D,Bt]}throw[0,D,Br]}throw[0,D,Bo]}c(ap[19],Bm,x);var
j5=[0,x];aY(780,j5,"Micromega_plugin.G_micromega");aY(781,[0,gV,l,s,p,B,aF,cW,$,j5],"Micromega_plugin");return});
