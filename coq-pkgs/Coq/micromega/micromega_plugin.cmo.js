(function(Bs){"use strict";var
gQ="QMicromega",gT=108,k0="__varmap",lH="__p",lh="__p%i",lG="__ff",ky="=",bF=163,gW=">=",cs=" * ",M="micromega",lF="enum_proof\n",fc='command "',kZ="( )^2",aC=148,kY="<>",gZ="real_nonlinear_prover",lE="(%a)-(%a)",kx=" [*] ",lf="__wit",lg=" ,",fj=115,kw="Zero",s=120,fi="Lia",kv=144,lD="scale_term : not implemented",kX="]",gV=117,bq=166,kW=" \n",ku="RED",d$=150,le='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',lC="parse_zop",lB="Zmicromega",kV="0",lA=")-(",aP=136,a5=248,gS="%a * %a",kt="Sos_Q",ld=126,ks="Sos_Z",lc="micromega_plugin",lz=">",ec=" + ",kU=156,B="Coq",kS=131,kT="LRA_Q",ff=112,lb=")+(",kQ="NQA",kR="AProof : ",kr="<linear_prover",kP="LRA_R",gY="$i",am="$t",fe=182,k$="[",la="CProof : %a",R="Tauto",bD=132,kO="%a + %a",k_=" Cannot find witness",kN="the use of a specialized external tool called csdp. \n\n",gP="PsatzZ",ly="Rdefinitions",kM="Timeout",k9="D",k8=" Skipping what remains of this tactic: the complexity of the goal requires ",lx="A(",kK="psatz_Q",kL='" exited ',bG=109,kI="psatz_Z",kJ="Rpow_def",kH="nat",k7="PsatzQ",lw=205,gO="pure_sos",fh=195,ak="VarMap",kq="%i ",kG="(%a)+(%a)",lu="}",lv="monoid",lt=214,k=250,k6="PsatzR",kF="buggy certificate",X="ZMicromega",ko="NRA",kp="C0",bZ="plugins/micromega/certificate.ml",kn="compare_num",d=246,ls="Nia",bY=165,al="Extension: cannot occur",k5=204,eb=113,km="ZArithRing",fb=208,kE="{",kl="AProof : %a\n",kD="",bE="RingMicromega",kk="C1",gU="real nonlinear prover",gR=100,lr="%a %s %s",lq="=<",k4="positive",kj="__arith",ea="Reals",kg=170,kh=", ",gN="<=",ki="QArith",fa=438,kC=" -> ",lp="psatz_R",gX="[%a]",kB="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",lo="Bad logical fragment",kf=215,W="plugins/micromega/g_micromega.ml4",gM="plugins/micromega/mfourier.ml",kA=206,ln=153,e$=127,k3="CProof : ",k2=133,e_=196,lm="EnvRing",k1="Refl",ke="t",fd=209,lk="linear prover",ll="Depth",lj="Sos_R",fg=114,d_=147,gL="%i",kz=")^(",li="}\n",J=Bs.jsoo_runtime,z=J.caml_check_bound,V=J.caml_equal,kb=J.caml_float_compare,a4=J.caml_fresh_oo_id,$=J.caml_int_compare,kd=J.caml_int_of_string,gK=J.caml_lessthan,Bp=J.caml_list_of_js_array,kc=J.caml_ml_string_length,e9=J.caml_mul,c=J.caml_new_string,j=J.caml_obj_tag,a3=J.caml_register_global,w=J.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):J.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):J.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):J.caml_call_gen(a,[b,c,d])}function
H(a,b,c,d,e){return a.length==4?a(b,c,d,e):J.caml_call_gen(a,[b,c,d,e])}function
aO(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):J.caml_call_gen(a,[b,c,d,e,f])}function
D(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):J.caml_call_gen(a,[b,c,d,e,f,g])}function
d9(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):J.caml_call_gen(a,[b,c,d,e,f,g,h])}function
bX(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):J.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Br(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):J.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
Bq(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):J.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
x=J.caml_get_global_data(),ep=[0,0,0],Bo=[12,44,[2,0,[11,c(")\n"),0]]],ey=[0,0],c3=[0,0,0],i6=[0,c(B),[0,c("Logic"),[0,c("Decidable"),0]]],gk=Bp([[0,c(B),[0,c("Lists"),[0,c("List"),0]]],[0,c(X),0],[0,c(R),0],[0,c(bE),0],[0,c(lm),0],[0,c(B),[0,c(M),[0,c(X),0]]],[0,c(B),[0,c(M),[0,c("RMicromega"),0]]],[0,c(B),[0,c(M),[0,c(R),0]]],[0,c(B),[0,c(M),[0,c(bE),0]]],[0,c(B),[0,c(M),[0,c(lm),0]]],[0,c(B),[0,c(ki),[0,c("QArith_base"),0]]],[0,c(B),[0,c(ea),[0,c(ly),0]]],[0,c(B),[0,c(ea),[0,c(kJ),0]]],[0,c("LRing_normalise"),0]]),i8=[0,[0,c(B),[0,c("Numbers"),[0,c("BinNums"),0]]],0],i9=[0,[0,c(B),[0,c(ea),[0,c(ly),0]]],[0,[0,c(B),[0,c(ea),[0,c(kJ),0]]],[0,[0,c(B),[0,c(ea),[0,c("Raxioms"),0]]],[0,[0,c(B),[0,c(ki),[0,c("Qreals"),0]]],0]]]],i_=[0,[0,c(B),[0,c("ZArith"),[0,c("BinInt"),0]]],0],j1=c(".csdp.cache"),y=c(lc),h=x.Pervasives,f=x.Num,m=x.Printf,q=x.Big_int,G=x.Unix,ej=x.Marshal,ei=x.Printexc,g=x.List,br=x.Hashtbl,F=x.Assert_failure,g4=x.Ratio,fk=x.Failure,eh=x.Set,O=x.Not_found,bO=x.Map,bv=x.CErrors,iO=x.String,i=x.CamlinternalLazy,I=x.Names,o=x.Term,Q=x.Loc,aj=x.Tactics,ae=x.Tacmach,L=x.Tacticals,bA=x.Pp,e4=x.Proofview,E=x.Coqlib,jK=x.Termops,jx=x.Invalid_argument,aM=x.Constr,js=x.Retyping,gi=x.Goptions,r=x.Ltac_plugin,cr=x.Stdarg,U=x.Genarg,as=x.Mltop,aB=x.Array,lI=c(kV),lJ=[0,[12,118,[2,0,0]],c("v%s")],lK=[0,[11,c("1/("),[15,[12,41,0]]],c("1/(%a)")],lL=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],lM=[0,[12,40,[15,[11,c(lb),[15,[12,41,0]]]]],c(kG)],lN=[0,[12,40,[15,[11,c(lA),[15,[12,41,0]]]]],c(lE)],lO=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],lP=[0,[12,40,[15,[11,c(")/("),[15,[12,41,0]]]]],c("(%a)/(%a)")],lQ=[0,[12,40,[15,[11,c(kz),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],lR=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],lS=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],lT=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],lU=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],lV=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],lW=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],lX=[0,[12,40,[15,[11,c(")^2"),0]]],c("(%a)^2")],lY=[0,[11,c(lv),0],c(lv)],lZ=[0,[15,[11,c(cs),[15,0]]],c(gS)],l0=[0,[15,[11,c(ec),[15,0]]],c(kO)],l1=[0,[15,[11,c(cs),[15,0]]],c(gS)],l4=c(";"),ma=c("map3"),mF=[0,[11,c(fc),[2,0,[11,c(kL),[4,3,0,0,0]]]],c('command "%s" exited %i')],mE=[0,[11,c(fc),[2,0,[11,c(kL),[2,0,0]]]],c('command "%s" exited %s')],mG=[0,[11,c(fc),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],mH=[0,[11,c(fc),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],mv=[0,c("plugins/micromega/mutils.ml"),287,7],mp=c("select_pos"),mi=[0,0,0],mf=c("list_fold_right_elements"),md=c("try_find"),l7=c("from_option"),nV=[0,0],n6=[0,0,0],n7=[0,[0,0],0],nP=[0,0],nO=[0,0],nN=[0,0],nF=[0,[0,0]],nG=[0,[0,0]],nH=[0,[0,0]],nI=[0,[0,0]],nB=[0,[0,0]],nC=[0,[0,0]],nD=[0,[0,0]],nE=[0,[0,0]],ns=[0,[0,0],0],nq=[0,0,0],nj=[0,1],nk=[0,2],nl=[0,3],ne=[0,0],ng=[0,0],nf=[0,1],ni=[0,3],nh=[0,0],mW=[0,[1,0]],mX=[0,0,[0,0]],mY=[0,[0,0],0],mZ=[0,0],m0=[0,[1,0]],m1=[0,[1,0]],m2=[0,0],m3=[0,[1,0]],m4=[0,[1,0]],m5=[0,[1,0]],m6=[0,0],m7=[0,[1,0]],m8=[0,0,0],m9=[0,0,0],m_=[0,0],m$=[0,0,0],na=[0,0],mS=[1,0],mR=[0,0],mJ=[1,0],mK=[1,0],mL=[0,0],mN=[0,0],mM=[0,0],nx=[0,0],nA=[0,0],nT=[0,0],nX=[0,[0,0],0],nY=[0,0,0],nZ=[0,[0,0],0],n0=[0,0,0],n1=[0,[0,0],0],n2=[0,0,0],n3=[0,0,0],n4=[0,0,0],n8=[0,[0,0],0],n9=[0,0,0],n_=[0,[0,0],0],n$=[0,0,0],oa=[0,[0,0],0],ob=[0,0,0],oc=[0,0,0],od=[0,0,0],oZ=[0,[11,c(kw),0],c(kw)],o0=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],o1=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],o2=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],o3=[0,[11,c(kZ),0],c(kZ)],o4=[0,[11,c("P * "),[15,0]],c("P * %a")],o5=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],o6=[0,[15,[11,c(cs),[15,0]]],c(gS)],o7=[0,[15,[11,c(ec),[15,0]]],c(kO)],o8=[0,[12,91,[15,[12,93,0]]],c(gX)],o9=[0,[12,46,0],c(".")],o_=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(" ; "),[15,0]]]]],c("%i:= %a ; %a")],o$=[0,[4,3,0,0,[12,123,[15,[11,c(gN),[15,[11,c(gN),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],pp=[0,0],po=[0,[11,c("apply_pivot -> {"),[15,[11,c(li),0]]],c("apply_pivot -> {%a}\n")],pn=[0,[11,c("xpivot_eq {"),[15,[11,c("} "),[15,[12,32,[2,0,[11,c(" {"),[15,[11,c(li),0]]]]]]]]],c("xpivot_eq {%a} %a %s {%a}\n")],pm=[0,0],pl=[0,[11,c("mult "),[2,0,[12,32,[15,[11,c(" ("),[15,[12,44,[2,0,[11,c(") -> ("),[15,Bo]]]]]]]]]],c("mult %s %a (%a,%s) -> (%a,%s)\n")],pk=[0,0],pj=[0,0,[0,0]],pi=[0,0,[0,0]],pg=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c(lr)],pf=[0,[2,0,[12,46,[15,[11,c(" +"),0]]]],c("%s.%a +")],pc=[0,c("plugins/micromega/polynomial.ml"),542,10],pb=[0,[11,c("normalise_proof "),[15,[11,c(kC),[15,0]]]],c("normalise_proof %a -> %a")],oW=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c(lr)],oU=c(gW),oT=c(ky),oL=c(kn),oM=c(kn),oP=[0,0],oK=[0,0],oH=[0,0],oF=[0,[2,0,[12,s,[4,3,0,0,[11,c(ec),0]]]],c("%sx%i + ")],ow=[0,0],ov=[0,1],ot=[0,0],or=[0,[2,0,[12,32,0]],c("%s ")],os=[0,[2,0,[12,42,[15,[12,32,0]]]],c("%s*%a ")],oh=[0,[12,s,[4,3,0,0,[12,46,0]]],c("x%i.")],oi=[0,[12,s,[4,3,0,0,[12,94,[4,3,0,0,[12,46,0]]]]],c("x%i^%i.")],oI=[0,1],pA=[0,[12,72,[4,3,0,0,0]],c("H%i")],pB=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],pC=[0,[11,c(lx),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],qD=[0,1],qE=[0,[0,0,0]],qA=c("merge_proof : pivot is not possible"),qB=[0,1],qC=[0,1],qz=[0,0],qt=[0,1],qu=[0,1],qv=[0,1],qw=[0,1],qx=[0,1],qy=[0,1],qp=[0,0],qq=[0,-1],qr=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],qn=[0,0,0],qe=[0,0],qf=[0,0],qg=[0,0],qh=[0,0],qi=[0,0],qj=[0,0],qk=[0,0],ql=[0,0],qd=[0,0],qc=c("bound_of_variable: impossible"),qb=[0,0,0],p$=[0,0,0],qa=c("SystemContradiction"),p_=[0,0],p9=[0,[4,3,0,0,[11,c(kC),[2,0,[12,10,0]]]],c("%i -> %s\n")],p6=[0,0],p5=[0,0,0,0],p3=[0,0],p1=[0,0],p2=[0,0],p4=[0,c(gM),309,4],p0=[0,c(gM),261,9],pX=[0,1],pY=[0,0],pU=[0,c(gM),199,4],pT=[0,[11,c("(val x = "),[2,0,[11,c(lg),[15,[12,44,[2,0,[12,41,0]]]]]]],c("(val x = %s ,%a,%s)")],pO=[0,[2,0,[11,c(" <= "),0]],c("%s <= ")],pP=[0,[11,c(gN),[2,0,[12,10,0]]],c("<=%s\n")],pQ=c("\n"),pL=[0,[4,3,0,0,[12,32,0]],c(kq)],pK=c(kE),pM=c(lu),pH=[0,[4,3,0,0,[12,32,0]],c(kq)],pG=c(kE),pI=c(lu),pE=[0,[12,40,[15,[12,44,[15,[12,41,0]]]]],c("(%a,%a)")],pD=c("oo"),pv=[0,1],py=c("Mfourier.SystemContradiction"),pW=c("Mfourier.TimeOut"),ru=c(lD),rv=c("scale term: not implemented"),rw=c(lD),rz=[0,0],rA=c("term_to_q_expr: not implemented"),rF=[0,0],rG=c("term_to_z_expr: not implemented"),rM=c("Cuts should already be compiled"),sm=[0,0],sn=[0,1],so=[0,0],sp=[0,1],sq=[0,c(bZ),1276,1],sd=[0,[11,c(lF),0],c(lF)],sf=[0,c(bZ),1175,2],se=[0,[11,c("Found interval: "),[15,[11,c(" in ["),[2,0,[12,59,[2,0,[11,c("] -> "),0]]]]]]],c("Found interval: %a in [%s;%s] -> ")],sg=[0,0],sh=[0,1],sk=[0,c(bZ),1202,2],si=[0,[11,c("xlia:  "),[15,[11,c(kW),0]]],c("xlia:  %a \n")],sj=[0,[11,c("after reduction:  "),[15,[11,c(kW),0]]],c("after reduction:  %a \n")],sb=[0,[11,c("Found a new bound "),[15,0]],c("Found a new bound %a")],r$=[0,1],sa=[0,0,0],sc=c("Interval without proof"),r_=[0,[11,c("Bad news : loss of completeness "),[15,[12,61,[2,0,0]]]],c("Bad news : loss of completeness %a=%s")],r8=[0,0],r6=[0,1],r7=[0,-1],r4=[0,1],r5=[0,-1],r0=[0,[2,0,[11,c(cs),[2,0,[11,c(ec),[2,0,[11,c(cs),[2,0,[11,c(" = "),[2,0,[12,10,0]]]]]]]]]],c("%s * %s + %s * %s = %s\n")],rV=[0,1],rW=[0,c(bZ),817,5],rX=[0,0],rT=[0,[11,c(kr),0],c(kr)],rU=[0,[12,62,0],c(lz)],rS=c("proof_of_farkas : not enough hyps"),rO=[0,[11,c(kR),[15,[12,10,0]]],c(kl)],rP=[0,[11,c(k3),[15,0]],c(la)],rN=[0,[11,c("compiled proof "),[15,[12,10,0]]],c("compiled proof %a\n")],rL=[0,[0,0]],rK=c("id_of_hyp"),rI=[0,0],rJ=[0,1],rE=[0,0],rB=[0,1],rC=[0,0],ry=c("bad index"),rt=c("pexpr_of_cstr_compat"),rr=[0,c(bZ),512,9],rp=[0,c(bZ),493,12],ri=[0,0],re=c("cannot happen"),rb=[0,[11,c(kR),[15,[12,10,0]]],c(kl)],rc=[0,[11,c(k3),[15,0]],c(la)],ra=[0,[11,c("raw certificate "),[2,0,0]],c("raw certificate %s")],q7=c("make_certificate(1)"),q8=c("empty_certificate"),q2=c("= 0"),q3=c("<> 0"),q4=c("> 0"),q5=c(">= 0"),q0=[0,1],qZ=[0,0],qU=[0,0],qV=[0,[0,0]],qW=[0,0],qY=[0,c(bZ),gR,1],qX=[0,0],qT=[0,[0,0]],qS=[0,[0,0]],qK=[0,0],qP=[0,[0,0],0],qQ=[0,0,0],q9=c("Certificate.Found"),q$=c("Certificate.Strict"),rY=c("Certificate.FoundProof"),r1=c("Certificate.Result"),sz=[0,0,0],sx=[0,0,0],sv=[0,0,[0,5,0]],sy=[0,1,[0,4,[0,5,0]]],sw=[0,1,[0,6,[0,5,0]]],st=[0,1,[0,6,[0,5,0]]],sr=c("Persistent_cache.PHashtable(Key).InvalidTableFormat"),ss=c("Persistent_cache.PHashtable(Key).UnboundTable"),sM=c("tt"),sN=c("ff"),sO=c("X "),sP=[0,[11,c(lx),[15,[12,41,0]]],c("A(%a)")],sQ=[0,[11,c("C("),[15,[12,44,[15,[12,41,0]]]]],c("C(%a,%a)")],sR=[0,[11,c("D("),[15,[12,44,[15,[12,41,0]]]]],c("D(%a,%a)")],sS=[0,[11,c("N("),[15,[12,41,0]]],c("N(%a)")],sU=c(kD),sT=[0,[11,c("I("),[15,[2,0,[12,44,[15,[12,41,0]]]]]],c("I(%a%s,%a)")],xl=[0,0],xz=c("[]"),xA=[0,[12,91,[15,[12,93,0]]],c(gX)],xB=[0,[12,91,[15,[11,c(kh),[15,[11,c(kh),[15,[12,93,0]]]]]]],c("[%a, %a, %a]")],xD=[0,[12,68,0],c(k9)],xE=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],xF=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],xG=c(kX),xH=c(k$),xI=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],xV=c("abstract_wrt_formula"),yV=c(gZ),yT=c(gZ),yQ=c(gZ),yK=c(gU),yJ=c(gU),yI=c(gU),yt=c(kF),ys=c(kF),ym=c("csdpcert"),yn=c(M),yo=c("plugins"),yg=c(kj),yh=[0,0],yi=c(k_),ya=c(lo),yb=c(kM),yc=c(kB),yd=c(le),ye=c(kN),yf=c(k8),x6=c(lf),x7=c(ke),x8=[0,[0,c(B),[0,c(M),[0,c(ak),0]]],[0,[0,c(ak),0],0]],x9=c(ak),x_=c(k0),x$=c(lG),x3=c(kj),x4=[0,0],x5=c(k_),xX=c(lo),xY=c(kM),xZ=c(kB),x0=c(le),x1=c(kN),x2=c(k8),xT=c("bad old index"),xU=c("proof compaction error"),xR=[0,[15,[11,c(" ;"),0]],c("%a ;")],xQ=c(k$),xS=c(kX),xO=[0,0],xL=c(lf),xM=c(k0),xN=c(lG),xC=[0,[15,[12,47,[15,0]]],c("%a/%a")],xw=c(ke),xx=[0,[0,c(B),[0,c(M),[0,c(ak),0]]],[0,[0,c(ak),0],0]],xy=c(ak),xt=c("Empty"),xu=[0,[0,c(B),[0,c(M),[0,c(ak),0]]],[0,[0,c(ak),0],0]],xv=c(ak),xq=c("Leaf"),xr=[0,[0,c(B),[0,c(M),[0,c(ak),0]]],[0,[0,c(ak),0],0]],xs=c(ak),xn=c("Node"),xo=[0,[0,c(B),[0,c(M),[0,c(ak),0]]],[0,[0,c(ak),0],0]],xp=c(ak),v1=c(kp),v2=c(kk),v3=c("CQ _"),v4=[0,[12,40,[15,[11,c(ec),[15,[12,41,0]]]]],c("(%a + %a)")],v5=[0,[12,40,[15,[11,c(" - "),[15,[12,41,0]]]]],c("(%a - %a)")],v6=[0,[12,40,[15,[11,c(cs),[15,[12,41,0]]]]],c("(%a * %a)")],v7=[0,[11,c("(/ "),[15,[12,41,0]]],c("(/ %a)")],v8=[0,[11,c("(- "),[15,[12,41,0]]],c("(- %a)")],w1=[0,0,0],xi=[0,[11,c(lH),[4,3,0,0,0]],c(lh)],xh=[0,[11,c(lH),[4,3,0,0,0]],c(lh)],xg=[0,[11,c("__x"),[4,3,0,0,0]],c("__x%i")],w7=c("error : parse_arith(2)"),w5=c("parse_qexpr parse error"),w3=[0,0],wN=c("get_rank"),wK=[1,c("Oups")],wI=c(lC),wH=c(lC),wD=[0,[12,40,[15,[12,32,[15,[12,32,[15,[12,41,0]]]]]]],c("(%a %a %a)")],ww=[0,[12,61,0],c(ky)],wx=[0,[11,c(kY),0],c(kY)],wy=[0,[11,c(lq),0],c(lq)],wz=[0,[11,c(gW),0],c(gW)],wA=[0,[12,60,0],c("<")],wB=[0,[12,62,0],c(lz)],wp=[0,[12,48,0],c(kV)],wq=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c(kH),0]]]]],c("(In %a)%%nat")],wr=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],ws=[0,[11,c("( "),[15,[11,c(kx),[15,[12,41,0]]]]],c("( %a [*] %a)")],wt=[0,[12,40,[15,[11,c(kx),[15,[12,41,0]]]]],c("(%a [*] %a)")],wu=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],wv=[0,[12,40,[15,[12,41,[12,37,[11,c(k4),0]]]]],c("(%a)%%positive")],wm=[0,[12,91,[15,[12,93,0]]],c(gX)],wl=[0,[12,40,[15,[12,32,[12,64,[15,[12,41,0]]]]]],c("(%a @%a)")],wh=[0,[11,c("Pc "),[15,0]],c("Pc %a")],wi=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],wj=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],wb=[0,[11,c("V "),[15,0]],c("V %a")],wc=[0,[12,40,[15,[11,c(lb),[15,[12,41,0]]]]],c(kG)],wd=[0,[12,40,[15,[11,c(lA),[15,[12,41,0]]]]],c(lE)],we=[0,[15,[11,c("*("),[15,[12,41,0]]]],c("%a*(%a)")],wf=[0,[11,c("-("),[15,[12,41,0]]],c("-(%a)")],wg=[0,[12,40,[15,[11,c(kz),[15,[12,41,0]]]]],c("(%a)^(%a)")],v_=[0,[15,[11,c(lg),[15,0]]],c("%a ,%a")],v$=[0,[15,0],c("%a")],wa=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],vZ=[0,[2,0,0],c("%s")],vX=[0,[4,3,0,0,0],c(gL)],vV=[0,[4,3,0,0,0],c(gL)],vU=[0,[4,3,0,0,0],c(gL)],vQ=c("ukn"),vR=c("BadTerm"),vS=c("Goal"),vL=c("Formula"),vM=[0,[0,c(B),[0,c(M),[0,c(bE),0]]],[0,[0,c(bE),0],0]],vN=c(bE),vI=c("Build_Formula"),vJ=[0,[0,c(B),[0,c(M),[0,c(bE),0]]],[0,[0,c(bE),0],0]],vK=c(bE),vE=c("N_of_Z"),vF=[0,[0,c(B),[0,c("setoid_ring"),[0,c(km),0]]],0],vG=c(km),vA=c("ZWitness"),vB=[0,[0,c(B),[0,c(M),[0,c(X),0]]],0],vC=c(gQ),vw=c("QWitness"),vx=[0,[0,c(B),[0,c(M),[0,c(gQ),0]]],0],vy=c(gQ),vs=c("BFormula"),vt=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vu=c(X),vp=c("I"),vq=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vr=c(X),vm=c("X"),vn=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vo=c(X),vj=c("A"),vk=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vl=c(X),vg=c("N"),vh=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vi=c(X),vd=c(k9),ve=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vf=c(X),va=c("Cj"),vb=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],vc=c(X),u9=c("FF"),u_=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],u$=c(X),u6=c("TT"),u7=[0,[0,c(B),[0,c(M),[0,c(R),0]]],[0,[0,c(R),0],0]],u8=c(X),u2=c("make_conj"),u3=[0,[0,c(k1),0],0],u4=c(lB),uY=c("make_impl"),uZ=[0,[0,c(k1),0],0],u0=c(lB),uW=c("coneMember"),uV=c(gP),uU=c("PsatzC"),uT=c("PsatzAdd"),uS=c("PsatzMulC"),uR=c("PsatzMulE"),uQ=c("PsatzSquare"),uP=c("PsatzIn"),uO=c("OpGt"),uN=c("OpGe"),uM=c("OpLt"),uL=c("OpLe"),uK=c("OpNEq"),uJ=c("OpEq"),uI=c("Pinj"),uH=c("Pc"),uG=c("PX"),uF=c("PEpow"),uE=c("PEsub"),uD=c("PEmul"),uC=c("PEopp"),uB=c("PEadd"),uA=c("PEc"),uz=c("PEX"),uy=c("Q2R"),ux=c("IZR"),uw=c("pow"),uv=c("Rinv"),ut=c("Rdiv"),us=c("Rmult"),ur=c("Ropp"),uq=c("Rminus"),up=c("Rplus"),uo=c("Rlt"),un=c("Rle"),um=c("Rge"),ul=c("Rgt"),uk=c("Qpower"),uj=c("Qmult"),ui=c("Qopp"),uh=c("Qminus"),ug=c("Qplus"),uf=c("Qeq"),ue=c("Qlt"),ud=c("Qle"),ub=c("Qge"),t$=c("Qgt"),t_=c("Z.pow"),t9=c("Z.mul"),t8=c("Z.opp"),t7=c("Z.sub"),t6=c("Z.add"),t5=c("eq"),t4=c("Z.lt"),t3=c("Z.le"),t2=c("Z.ge"),t1=c("Z.gt"),tZ=c("EnumProof"),tX=c("CutProof"),tV=c("RatProof"),tT=c("DoneProof"),tR=c("ZArithProof"),tQ=c("R1"),tP=c("R0"),tO=c("COpp"),tN=c("CInv"),tM=c("CMult"),tL=c("CMinus"),tK=c("CPlus"),tJ=c("CZ"),tI=c("CQ"),tH=c(kk),tG=c(kp),tE=c("Rcst"),tD=c("Qmake"),tB=c("Build_Witness"),tA=c("R"),tz=c("Q"),ty=c("Zneg"),tx=c("Zpos"),tw=c("Z0"),tv=c("Z"),tu=c("xI"),tt=c("xO"),ts=c("xH"),tq=c(k4),to=c("option"),tm=c("None"),tl=c("pair"),tk=c("Npos"),tj=c("N0"),th=c(kH),tg=c("S"),tf=c("O"),td=c("list"),tc=c("nil"),tb=c("cons"),ta=c("False"),s$=c("True"),s_=c("iff"),s9=c("not"),s8=c("or"),s7=c("and"),sW=[0,0,0],sD=c(kD),sC=[0,[11,c("time "),[2,0,[12,32,[8,0,0,0,[12,10,0]]]]],c("time %s %f\n")],sF=[0,c(fi),[0,c("Enum"),0]],sG=c("Lia Enum"),sI=[0,c("Lra"),[0,c(ll),0]],sK=[0,c(fi),[0,c(ll),0]],s1=c(X),s2=c(X),s3=c(X),s4=c(X),s5=c(X),s6=c(X),vT=c("Coq_micromega.M.ParseError"),xW=c("Coq_micromega.CsdpNotFound"),yj=c("csdp"),yv=c(".lia.cache"),yx=c(".nia.cache"),yz=c(".nra.cache"),yC=c(lk),yF=c(lk),yH=c("nra"),yL=c("lia"),yM=c("nlia"),yW=c(gO),yZ=c(gO),y2=c(gO),Bl=[0,c(W),1,0],Bj=[0,c(W),1,0],Bg=[0,c(W),1,0],Bf=c(am),Bh=[0,c(kK)],Bi=c(am),Bk=c(gY),Bm=[0,c(kK)],Bn=c(k7),Ba=c(al),A_=c(al),A6=[0,c(W),1,0],A4=[0,c(W),1,0],A1=[0,c(W),1,0],A0=c(am),A2=[0,c(lp)],A3=c(am),A5=c(gY),A7=[0,c(lp)],A8=c(k6),AV=c(al),AT=c(al),AP=[0,c(W),1,0],AO=c(am),AQ=[0,c("lra_R")],AR=c(kP),AJ=c(al),AF=[0,c(W),1,0],AE=c(am),AG=[0,c("lra_Q")],AH=c(kT),Az=c(al),Av=[0,c(W),1,0],Au=c(am),Aw=[0,c("sos_R")],Ax=c(lj),Ap=c(al),Al=[0,c(W),1,0],Ak=c(am),Am=[0,c("sos_Q")],An=c(kt),Af=c(al),Ab=[0,c(W),1,0],Aa=c(am),Ac=[0,c("sos_Z")],Ad=c(ks),z7=c(al),z3=[0,c(W),1,0],z2=c(am),z4=[0,c("xnqa")],z5=c(kQ),zX=c(al),zT=[0,c(W),1,0],zS=c(am),zU=[0,c("xnra")],zV=c(ko),zN=c(al),zJ=[0,c(W),1,0],zI=c(am),zK=[0,c("xnlia")],zL=c(ls),zD=c(al),zz=[0,c(W),1,0],zy=c(am),zA=[0,c("xlia")],zB=c(fi),zt=c(al),zp=[0,c(W),1,0],zn=[0,c(W),1,0],zk=[0,c(W),1,0],zj=c(am),zl=[0,c(kI)],zm=c(am),zo=c(gY),zq=[0,c(kI)],zr=c(gP),ze=c(al),zc=c(al),y9=c(ku),y_=c("myred"),y7=c(lc),za=c(ku),zh=c(gP),zw=c(fi),zG=c(ls),zQ=c(ko),z0=c(kQ),z_=c(ks),Ai=c(kt),As=c(lj),AC=c(kT),AM=c(kP),AY=c(k6),Bd=c(k7),su=x.End_of_file,yl=x.Coq_config,yp=x.Envars,yq=x.Filename,wG=x.Reductionops,yk=x.System;function
an(d,c){if(typeof
c==="number")return b(h[49],d,lI);else
switch(c[0]){case
0:var
n=a(f[40],c[1]);return b(h[49],d,n);case
1:return e(m[1],d,lJ,c[1]);case
2:return H(m[1],d,lK,an,c[1]);case
3:return H(m[1],d,lL,an,c[1]);case
4:var
g=c[1];return D(m[1],d,lM,an,g[1],an,g[2]);case
5:var
i=c[1];return D(m[1],d,lN,an,i[1],an,i[2]);case
6:var
j=c[1];return D(m[1],d,lO,an,j[1],an,j[2]);case
7:var
k=c[1];return D(m[1],d,lP,an,k[1],an,k[2]);default:var
l=c[1];return aO(m[1],d,lQ,an,l[1],l[2])}}function
b0(d,c){switch(c[0]){case
0:return e(m[1],d,lR,c[1]);case
1:return e(m[1],d,lS,c[1]);case
2:return e(m[1],d,lT,c[1]);case
3:var
g=a(f[40],c[1]);return e(m[1],d,lU,g);case
4:var
h=a(f[40],c[1]);return e(m[1],d,lV,h);case
5:var
i=a(f[40],c[1]);return e(m[1],d,lW,i);case
6:return H(m[1],d,lX,an,c[1]);case
7:return b(m[1],d,lY);case
8:return D(m[1],d,lZ,an,c[1],b0,c[2]);case
9:return D(m[1],d,l0,b0,c[1],b0,c[2]);default:return D(m[1],d,l1,b0,c[1],b0,c[2])}}var
g0=[0,an,b0];a3(726,g0,"Micromega_plugin.Sos_types");var
l2=0;function
l3(e,c,d){var
a=d;for(;;){if(a){var
f=a[2];b(e,c,a[1]);b(h[49],c,l4);var
a=f;continue}return 0}}function
g1(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
l5(c,b){return b?[0,a(c,b[1])]:0}function
l6(b){return b?b[1]:a(h[2],l7)}function
l8(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1][1],d);if(c)return c;var
b=f;continue}return 0}}function
l9(e,d){var
c=0,a=d;for(;;){if(a){var
f=a[2];b(e,c,a[1]);var
c=c+1|0,a=f;continue}return 0}}function
l_(h,f){var
c=0,a=f;for(;;){if(a){var
d=a[2],j=a[1],i=function(d){return function(c,a){return[0,b(h,d,a),c]}}(j),c=e(g[16],i,c,d),a=d;continue}return c}}function
l$(f,d){var
c=0,a=d;for(;;){if(a){var
i=a[2],j=a[1],h=function(d){return function(c,a){return[0,b(f,d,a),c]}}(j),c=e(g[16],h,c,a),a=i;continue}return c}}function
g2(f,d,c,b){if(d){if(c)if(b){var
g=b[1],i=c[1],j=d[1],k=g2(f,d[2],c[2],b[2]);return[0,e(f,j,i,g),k]}}else
if(!c)if(!b)return 0;return a(h[1],ma)}function
mb(g,f,e){var
c=f,a=e;for(;;){if(c){var
h=c[2],i=c[1];if(a){var
d=a[2];if(b(g,i,a[1])){var
c=h,a=d;continue}var
a=d;continue}return 0}return 1}}function
mc(c){return function(d){var
b=d;for(;;){if(b){var
e=b[2],f=b[1];try{var
g=a(c,f);return g}catch(a){a=w(a);if(a[1]===fk){var
b=e;continue}throw a}}return a(h[2],md)}}}function
me(g,c){function
d(c){if(c){var
e=c[2],f=c[1];return e?b(g,f,d(e)):f}return a(h[1],mf)}return d(c)}function
mg(e,d){var
a=[0,0,d];for(;;){var
b=a[2],c=a[1];if(b<e)return c;var
a=[0,[0,b,c],b-1|0];continue}}function
mh(h,b){function
c(e,b){var
c=e[2],d=e[1];if(d)return[0,d,[0,b,c]];var
f=a(h,b);return f?[0,[0,[0,f[1],b]],c]:[0,d,[0,b,c]]}return e(g[16],c,mi,b)}function
g3(d,c){var
a=b(q[17],d,c),e=b(q[15],d,a),f=b(q[15],c,a),g=b(q[10],e,f);return b(q[10],a,g)}function
ed(b){return 2===b[0]?a(g4[3],b[1]):q[2]}function
ee(b){switch(b[0]){case
0:return a(q[35],b[1]);case
1:return b[1];default:return a(g4[2],b[1])}}function
g5(d,c){var
b=d,a=c;for(;;){if(a){var
e=a[2],b=g3(b,ed(a[1])),a=e;continue}return b}}function
g6(e,d){var
c=e,a=d;for(;;){if(a){var
f=a[2],g=ee(a[1]),c=b(q[17],c,g),a=f;continue}return c}}function
mj(c){var
a=g6(q[1],c);return 0===b(q[23],a,q[1])?q[2]:a}function
mk(a){var
c=g5(q[2],a);function
d(a){var
d=ed(a),e=ee(a),f=b(q[10],e,c);return b(q[15],f,d)}return b(g[13],d,a)}function
fl(e,a){function
c(d,a){if(a){var
f=a[1],g=c(d+1|0,a[2]);return[0,b(e,f,d),g]}return 0}return c(0,a)}function
ml(c,b){var
d=fl(function(d,b){return[0,b,a(c,d)]},b);return a(g[6],d)}function
g7(c,b){var
d=c+a(g[1],b)|0;return[0,fl(function(b,a){return[0,b,a+c|0]},b),d]}function
mm(a){function
b(e,a){if(a){var
c=a[1],f=a[2],g=c[1],d=g7(e,c[2]),h=d[1];return[0,[0,g,h],b(d[2],f)]}return 0}return b(0,a)}function
mn(h,a){function
c(i){var
a=i;for(;;){if(a){var
d=a[2],e=a[1],f=e[2],j=e[1],k=function(a){return a[2]},l=b(g[13],k,f),m=function(a){return b(g[27],a,h)};if(b(g[24],m,l))return[0,[0,j,f],c(d)];var
a=d;continue}return 0}}return c(a)}function
mo(c,b){function
e(i,d,g){var
c=i,b=g;for(;;){if(d){var
j=d[2],k=d[1];if(b){var
f=b[2],l=b[1];if(c===k)return[0,l,e(c+1|0,j,f)];var
c=c+1|0,b=f;continue}return a(h[2],mp)}return 0}}return e(0,c,b)}function
g8(a){return a?g8(a[1])+1|0:0}function
b1(a){return typeof
a==="number"?1:0===a[0]?1+(2*b1(a[1])|0)|0:2*b1(a[1])|0}function
mq(a){return a?b1(a[1]):0}function
fm(a){return typeof
a==="number"?1:0===a[0]?1+(2*fm(a[1])|0)|0:2*fm(a[1])|0}function
mr(a){return typeof
a==="number"?0:0===a[0]?b1(a[1]):-b1(a[1])|0}function
ct(a){if(typeof
a==="number")return q[2];else{if(0===a[0]){var
c=ct(a[1]),d=b(q[11],2,c);return b(q[7],1,d)}var
e=ct(a[1]);return b(q[11],2,e)}}function
ef(b){if(typeof
b==="number")return q[1];else{if(0===b[0])return ct(b[1]);var
c=ct(b[1]);return a(q[3],c)}}function
ms(a){return[1,ef(a)]}var
mt=[0,g8,b1,mq,fm,mr,ct,ef,ms,function(a){var
c=a[1],d=[1,ef([0,a[2]])],e=[1,ef(c)];return b(f[9],e,d)}];function
g9(a){return 0===a?0:[0,g9(a-1|0)]}function
b2(a){return 1===a?0:1===(a&1)?[0,b2(a>>>1|0)]:[1,b2(a>>>1|0)]}function
mu(a){if(0<=a)return 0===a?0:[0,b2(a)];throw[0,F,mv]}function
fn(a){return 1===a?0:1===(a&1)?[0,fn(a>>>1|0)]:[1,fn(a>>>1|0)]}function
mw(a){var
b=$(a,0);return 0===b?0:1===b?[0,b2(a)]:[1,b2(-a|0)]}function
eg(d){var
f=a(q[35],2);function
c(a){if(b(q[24],a,q[2]))return 0;var
d=b(q[14],a,f),e=d[1];return b(q[24],q[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
g_(b){var
c=a(q[22],b);return 0===c?0:1===c?[0,eg(b)]:[1,eg(a(q[3],b))]}var
mx=[0,g9,b2,mu,fn,mw,eg,g_,function(a){var
b=eg(ed(a));return[0,g_(ee(a)),b]}];function
my(d){var
b=d;for(;;){if(b){var
e=b[2],c=a(b[1],0);if(0===c){var
b=e;continue}return c}return 0}}function
mz(g,f,e){var
c=f,a=e;for(;;){if(c){if(a){var
h=a[2],i=c[2],d=b(g,c[1],a[1]);if(0===d){var
c=i,a=h;continue}return d}return 1}return a?-1:0}}var
mA=[0,my,mz,function(e,d){var
b=d,c=0;for(;;){if(b){var
f=b[2],g=a(e,b[1])^c,b=f,c=g;continue}return c^a(br[20],0)}}];function
mB(a){return a}function
mC(a){return a+1|0}var
g$=[0,mB,mC,function(d,c){var
e=a(h[20],c);return b(h[49],d,e)},$],mD=a(eh[1],[0,g$[4]]);function
ha(a){for(;;)try{var
d=b(G[13],0,a)[2];return d}catch(a){a=w(a);if(a[1]===G[1]){var
c=a[2];if(typeof
c==="number")if(11===c)continue}throw a}}var
l=[0,l2,l3,g1,l5,l6,l8,l9,l_,l$,g2,mb,mc,me,mg,mh,g3,ed,ee,g5,g6,mj,mk,fl,ml,g7,mm,mn,mo,mt,mx,mA,g$,mD,ha,function(c,s,r){var
f=a(G[64],0),i=f[2],j=f[1],k=a(G[64],0),l=k[2],n=k[1],o=a(G[64],0),p=o[2],t=o[1],u=aO(G[66],c,s,j,l,p),q=a(G[29],i);b(h[55],q,r);a(h[46],q);var
d=ha(u);function
v(e){var
c=[0,j,[0,i,[0,n,[0,l,[0,t,[0,p,0]]]]]];function
d(b){try{var
c=a(G[22],b);return c}catch(a){return 0}}return b(g[11],d,c)}return g1(function(p){switch(d[0]){case
0:var
b=d[1];if(0===b){var
f=a(G[28],n);try{var
j=a(ej[3],f);return j}catch(b){b=w(b);var
g=a(ei[1],b),i=e(m[4],mE,c,g);return a(h[2],i)}}var
k=e(m[4],mF,c,b);return a(h[2],k);case
1:var
l=e(m[4],mG,c,d[1]);return a(h[2],l);default:var
o=e(m[4],mH,c,d[1]);return a(h[2],o)}},v)}];a3(737,l,"Micromega_plugin.Mutils");function
bH(a){return 0===a?1:0}function
ek(a,b){if(a){var
c=a[1];return[0,c,ek(a[2],b)]}return b}function
hb(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
fo(b,a){return b?[0,fo(b[1],a)]:a}var
mI=[0];function
a6(a){return typeof
a==="number"?mJ:0===a[0]?[1,a6(a[1])]:[0,a[1]]}function
b3(b,a){if(typeof
b==="number")return typeof
a==="number"?mK:0===a[0]?[1,a6(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,a6(c)]:0===a[0]?[1,cu(c,a[1])]:[0,b3(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,b3(d,a[1])]:[1,b3(d,a[1])]}}function
cu(b,a){if(typeof
b==="number")return typeof
a==="number"?mL:0===a[0]?[0,a6(a[1])]:[1,a6(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,a6(c)]:0===a[0]?[0,cu(c,a[1])]:[1,cu(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,a6(d)]:0===a[0]?[1,cu(d,a[1])]:[0,b3(d,a[1])]}}function
cv(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,cv(a[1])]}function
cw(a){return typeof
a==="number"?0===a?mM:1:[0,[0,a[1]]]}function
cx(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
hc(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,cv(a[1])]]}function
b4(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?cx(b4(c,a[1])):cw(b4(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,cv(d)]:0===a[0]?cw(cy(d,a[1])):cx(b4(d,a[1]))}}function
cy(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,cv(c)]:0===a[0]?cw(cy(c,a[1])):cx(b4(c,a[1]))}var
d=b[1];return typeof
a==="number"?hc(d):0===a[0]?cx(cy(d,a[1])):cw(cy(d,a[1]))}}function
fp(c,b){var
a=b4(c,b);return typeof
a==="number"?0:a[1]}function
fq(b,a){return typeof
b==="number"?a:0===b[0]?b3(a,[1,fq(b[1],a)]):[1,fq(b[1],a)]}function
cz(a){return typeof
a==="number"?mN:0===a[0]?[0,cz(a[1])]:[0,cz(a[1])]}function
hd(h,g,f){var
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
mO=0;function
he(a,b){return hd(mO,a,b)}function
fr(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(he(e,f)){case
0:return b;case
1:var
c=d,a=b,b=fp(f,e);continue;default:var
c=d,b=fp(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,fr(d,g,a[1])]}}}return 0}}function
mP(b,a){var
c=cz(a);return fr(fo(cz(b),c),b,a)}function
hf(a){return a?a6(hf(a[1])):0}var
A=[0,a6,b3,cu,cv,cw,cx,hc,b4,cy,fp,fq,cz,hd,he,fr,mP,hf],mQ=[0,function(b){return b?[0,a(A[17],b[1])]:0}];function
el(a,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=el(a,d,c[1]);return b(a,d,b(a,e,e))}var
f=el(a,d,c[1]);return b(a,f,f)}}function
hg(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
b5(c,b){if(b){var
d=b[1],e=b5(c,b[2]);return[0,a(c,d),e]}return 0}function
fs(d,c,a){if(a){var
e=a[1];return b(d,e,fs(d,c,a[2]))}return c}function
ft(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
hh(b){return typeof
b==="number"?mR:0===b[0]?[0,[0,b[1]]]:[1,a(A[4],b[1])]}function
hi(b){return typeof
b==="number"?mS:0===b[0]?[0,a(A[4],b[1])]:[1,[0,b[1]]]}function
bI(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?[1,[1,b[1]]]:[1,a(A[4],b[1])];else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?[0,[1,d]]:0===b[0]?ft(bI(d,b[1])):hh(bI(d,b[1]))}var
e=c[1];return typeof
b==="number"?[0,a(A[4],e)]:0===b[0]?hi(bI(e,b[1])):ft(bI(e,b[1]))}}function
bs(c,a){if(typeof
c==="number")return a;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?c:0===a[0]?[0,b(A[2],d,a[1])]:bI(d,a[1])}var
e=c[1];return typeof
a==="number"?c:0===a[0]?bI(a[1],e):[1,b(A[2],e,a[1])]}}function
bJ(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
em(b,a){return bs(b,bJ(a))}function
bK(c,a){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?0:0===a[0]?[0,b(A[11],d,a[1])]:[1,b(A[11],d,a[1])]}var
e=c[1];return typeof
a==="number"?0:0===a[0]?[1,b(A[11],e,a[1])]:[0,b(A[11],e,a[1])]}}function
cA(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
a!=="number"&&0===a[0])return b(A[14],d,a[1]);return 2}var
e=c[1];if(typeof
a!=="number"&&1===a[0])return hb(b(A[14],e,a[1]));return 1}}function
hj(b,a){return 2<=cA(b,a)?0:1}function
fu(b,a){return 1===cA(b,a)?1:0}function
mT(b,a){return 2<=cA(b,a)?1:0}function
mU(b,a){return 1===cA(b,a)?a:b}function
en(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
mV(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
bL(b,a){if(typeof
b==="number")return hj(mW,a)?mX:mY;else{if(0===b[0]){var
e=bL(b[1],a),f=e[1],c=bs(bK(m0,e[2]),mZ);if(fu(c,a))return[0,bK(m1,f),c];var
i=em(c,a);return[0,bs(bK(m3,f),m2),i]}var
g=bL(b[1],a),h=g[1],d=bK(m4,g[2]);if(fu(d,a))return[0,bK(m5,h),d];var
j=em(d,a);return[0,bs(bK(m7,h),m6),j]}}function
hk(b,a){if(typeof
b==="number")return m8;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return m9;else{if(0===a[0])return bL(c,a);var
d=bL(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,bJ(f),0];var
l=bs(a,e);return[0,bJ(bs(f,m_)),l]}}var
g=b[1];if(typeof
a==="number")return m$;else{if(0===a[0]){var
h=bL(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,bJ(j),0];var
m=em(a,i);return[0,bJ(bs(j,na)),m]}var
k=bL(g,[0,a[1]]),n=k[1];return[0,n,bJ(k[2])]}}}function
nb(b,a){return hk(b,a)[1]}var
u=[0,ft,hh,hi,bI,bs,bJ,em,bK,cA,hj,fu,mT,mU,en,mV,bL,hk,nb,function(c,a){if(typeof
c==="number")return en(a);else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?en(c):0===a[0]?[0,b(A[16],d,a[1])]:[0,b(A[16],d,a[1])]}var
e=c[1];return typeof
a==="number"?en(c):0===a[0]?[0,b(A[16],e,a[1])]:[0,b(A[16],e,a[1])]}}];function
aD(c,a){return 0===b(u[9],c,a)?1:0}function
nc(a){return[0,a]}function
nd(a){return[0,a]}function
fv(d,f,e){var
c=f,a=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===a[0]?b(d,g,a[1]):0;case
1:var
h=c[2],i=c[1];if(1===a[0]){var
j=a[2];if(0===b(A[14],i,a[1])){var
c=h,a=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===a[0]){var
n=a[3],o=a[1];if(0===b(A[14],l,a[2])){if(fv(d,m,o)){var
c=k,a=n;continue}return 0}return 0}return 0}}function
Y(c,a){switch(a[0]){case
0:return a;case
1:var
d=a[2];return[1,b(A[2],c,a[1]),d];default:return[1,c,a]}}function
hl(b,c){return typeof
b==="number"?c:0===b[0]?[1,[1,b[1]],c]:[1,a(A[4],b[1]),c]}function
N(f,e,a,d,c){switch(a[0]){case
0:return b(e,a[1],f)?Y(0,c):[2,a,d,c];case
1:return[2,a,d,c];default:var
g=a[2],h=a[1];return fv(e,a[3],[0,f])?[2,h,b(A[2],g,d),c]:[2,a,d,c]}}function
hm(c,b,a){return[2,[0,b],a,[0,c]]}function
hn(b,a){return hm(b,a,0)}function
af(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1];return[1,d,af(c,b[2])];default:var
e=b[2],f=b[1],g=af(c,b[3]);return[2,af(c,f),e,g]}}function
bt(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,bt(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,bt(d,a[3],c)]}}function
b6(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,b6(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,b6(d,a[3],c)]}}function
cB(g,f,e,c,d){switch(d[0]){case
0:return Y(c,bt(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=b(u[4],m,c);return typeof
h==="number"?Y(c,b(f,i,e)):0===h[0]?Y(c,b(f,[1,h[1],i],e)):Y(m,cB(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,b(f,j,e)]:0===c[0]?[2,l,k,cB(g,f,e,[1,c[1]],j)]:[2,l,k,cB(g,f,e,a(A[4],c[1]),j)]}}function
cC(h,g,f,e,c,d){switch(d[0]){case
0:var
o=d[1];return Y(c,bt(h,af(g,e),o));case
1:var
j=d[2],n=d[1],i=b(u[4],n,c);return typeof
i==="number"?Y(c,b(f,j,e)):0===i[0]?Y(c,b(f,[1,i[1],j],e)):Y(n,cC(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,b(f,k,e)]:0===c[0]?[2,m,l,cC(h,g,f,e,[1,c[1]],k)]:[2,m,l,cC(h,g,f,e,a(A[4],c[1]),k)]}}function
fw(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,a(A[4],h[1]),k]];default:var
l=c[3],m=c[2],n=c[1],i=b(u[4],m,e);return typeof
i==="number"?N(f,g,b(j,n,d),m,l):0===i[0]?N(f,g,b(j,[2,n,i[1],[0,f]],d),e,l):N(f,g,fw(f,g,j,d,i[1],n),m,l)}}function
fx(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,af(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,af(f,d),e,l];else{if(0===i[0]){var
p=[1,[1,i[1]],l];return[2,af(f,d),e,p]}var
q=[1,a(A[4],i[1]),l];return[2,af(f,d),e,q]}default:var
m=c[3],n=c[2],o=c[1],j=b(u[4],n,e);return typeof
j==="number"?N(g,h,b(k,o,d),n,m):0===j[0]?N(g,h,b(k,[2,o,j[1],[0,g]],d),e,m):N(g,h,fx(g,f,h,k,d,j[1],o),n,m)}}function
Z(c,e,d,f,g){switch(g[0]){case
0:return bt(e,f,g[1]);case
1:var
q=g[2],r=g[1];return cB(e,function(a,b){return Z(c,e,d,a,b)},q,r,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,bt(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,Z(c,e,d,m,h)]:0===k[0]?[2,i,j,Z(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,Z(c,e,d,[1,a(A[4],k[1]),m],h)];default:var
n=f[3],o=f[2],p=f[1],l=b(u[4],o,j);if(typeof
l==="number"){var
s=Z(c,e,d,n,h);return N(c,d,Z(c,e,d,p,i),o,s)}else{if(0===l[0]){var
t=l[1],v=Z(c,e,d,n,h);return N(c,d,Z(c,e,d,[2,p,t,[0,c]],i),j,v)}var
w=l[1],x=Z(c,e,d,n,h);return N(c,d,fw(c,d,function(a,b){return Z(c,e,d,a,b)},i,w,p),o,x)}}}}function
ag(d,f,g,c,e,h,i){switch(i[0]){case
0:return b6(g,h,i[1]);case
1:var
s=i[2],t=i[1];return cC(f,c,function(a,b){return ag(d,f,g,c,e,a,b)},s,t,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],w=bt(f,af(c,j),v);return[2,af(c,k),l,w];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
x=ag(d,f,g,c,e,o,j);return[2,af(c,k),l,x]}else{if(0===m[0]){var
y=ag(d,f,g,c,e,[1,[1,m[1]],o],j);return[2,af(c,k),l,y]}var
z=ag(d,f,g,c,e,[1,a(A[4],m[1]),o],j);return[2,af(c,k),l,z]}default:var
p=h[3],q=h[2],r=h[1],n=b(u[4],q,l);if(typeof
n==="number"){var
B=ag(d,f,g,c,e,p,j);return N(d,e,ag(d,f,g,c,e,r,k),q,B)}else{if(0===n[0]){var
C=n[1],D=ag(d,f,g,c,e,p,j);return N(d,e,ag(d,f,g,c,e,[2,r,C,[0,d]],k),l,D)}var
E=n[1],F=ag(d,f,g,c,e,p,j);return N(d,e,fx(d,c,e,function(a,b){return ag(d,f,g,c,e,a,b)},k,E,r),q,F)}}}}function
cD(f,e,d,a,c){switch(a[0]){case
0:return[0,b(e,a[1],c)];case
1:var
g=a[1];return Y(g,cD(f,e,d,a[2],c));default:var
h=a[2],i=a[1],j=cD(f,e,d,a[3],c);return N(f,d,cD(f,e,d,i,c),h,j)}}function
cE(d,g,f,c,e,a){return b(c,a,d)?[0,d]:b(c,a,g)?e:cD(d,f,c,e,a)}function
a7(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return Y(c,cE(f,j,i,e,d,h[1]));case
1:var
l=h[2],p=h[1],k=b(u[4],p,c);return typeof
k==="number"?Y(c,b(g,l,d)):0===k[0]?Y(c,b(g,[1,k[1],l],d)):Y(p,a7(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
c==="number"){var
q=b(g,m,d);return N(f,e,a7(f,j,i,e,g,d,0,o),n,q)}else{if(0===c[0]){var
r=a7(f,j,i,e,g,d,[1,c[1]],m);return N(f,e,a7(f,j,i,e,g,d,c,o),n,r)}var
s=a7(f,j,i,e,g,d,a(A[4],c[1]),m);return N(f,e,a7(f,j,i,e,g,d,c,o),n,s)}}}function
aa(b,e,f,d,c,g,h){switch(h[0]){case
0:return cE(b,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return a7(b,e,d,c,function(a,g){return aa(b,e,f,d,c,a,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return cE(b,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?aa(b,e,f,d,c,l,i):0===j[0]?aa(b,e,f,d,c,[1,[1,j[1]],l],i):aa(b,e,f,d,c,[1,a(A[4],j[1]),l],i);return N(b,c,aa(b,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=aa(b,e,f,d,c,n,i),u=0,v=a7(b,e,d,c,function(a,g){return aa(b,e,f,d,c,a,g)},i,u,p),w=aa(b,e,f,d,c,Y(0,n),k),x=aa(b,e,f,d,c,p,k),y=N(b,c,v,o,t);return Z(b,f,c,N(b,c,Z(b,f,c,N(b,c,x,o,[0,b]),w),m,[0,b]),y)}}}function
cF(a,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,b(f,h,h)];case
1:var
l=d[1];return[1,l,cF(a,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=aa(a,e,g,f,c,k,Y(0,cE(a,e,f,c,i,b(g,e,e)))),n=cF(a,e,g,f,c,i);return N(a,c,Z(a,g,c,N(a,c,cF(a,e,g,f,c,k),j,[0,a]),m),j,n)}}function
ho(c,b,a){return hl(a,hn(c,b))}function
cG(h,g,f,e,d,c,n,b,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return a(c,aa(h,g,f,e,d,j,b));else{if(0===i[0]){var
k=i[1];return a(c,aa(h,g,f,e,d,cG(h,g,f,e,d,c,cG(h,g,f,e,d,c,j,b,k),b,k),b))}var
l=i[1],j=cG(h,g,f,e,d,c,j,b,l),i=l;continue}}function
hp(h,a,g,f,e,d,c,b){return b?cG(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
ao(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return ho(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=ao(a,f,c,g,e,d,b,j[1]);return ag(a,c,e,d,b,ao(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=ao(a,f,c,g,e,d,b,i[1]);return ag(a,c,e,d,b,ao(a,f,c,g,e,d,b,j),l)}var
k=ao(a,f,c,g,e,d,b,i);return Z(a,c,b,ao(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=ao(a,f,c,g,e,d,b,h[2]);return ag(a,c,e,d,b,ao(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=ao(a,f,c,g,e,d,b,h[2]);return aa(a,f,c,g,b,ao(a,f,c,g,e,d,b,p),q);case
5:return af(d,ao(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=ao(a,f,c,g,e,d,b,h[1]);return hp(a,f,c,g,b,function(a){return a},s,r)}}function
a8(c,b){if(typeof
b==="number")switch(b){case
0:return 0;case
1:return 1;default:return 2}else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1],e=a8(c,b[2]);return[1,a8(c,d),e];case
2:var
f=b[1],g=a8(c,b[2]);return[2,a8(c,f),g];case
3:return[3,a8(c,b[1])];default:var
h=b[1],i=a8(c,b[2]);return[4,a8(c,h),i]}}var
eo=0;function
eq(e,d,c,f){if(f){var
h=f[2],g=f[1],i=b(d,c,g);if(i){if(a(e,i[1]))return 0;var
j=eq(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=eq(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=b(d,c,c);return l?a(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
hq(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=eq(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
hr(e,d,c,a){var
b=0;return fs(function(f,a){var
b=hq(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
cH(d,c,a,b){if(a){var
e=a[2],f=hr(d,c,a[1],b);return ek(cH(d,c,e,b),f)}return eo}function
er(b,a){return ek(b,a)}function
ap(d,c,f,e,p,o){var
b=p,g=o;for(;;)if(typeof
g==="number")switch(g){case
0:return b?eo:ep;case
1:return b?ep:eo;default:return ep}else
switch(g[0]){case
0:var
h=g[1];return b?a(f,h):a(e,h);case
1:var
i=g[2],j=g[1];if(b){var
q=ap(d,c,f,e,b,i);return er(ap(d,c,f,e,b,j),q)}var
r=ap(d,c,f,e,b,i);return cH(d,c,ap(d,c,f,e,b,j),r);case
2:var
k=g[2],l=g[1];if(b){var
s=ap(d,c,f,e,b,k);return cH(d,c,ap(d,c,f,e,b,l),s)}var
t=ap(d,c,f,e,b,k);return er(ap(d,c,f,e,b,l),t);case
3:var
u=g[1],b=bH(b),g=u;continue;default:var
m=g[2],n=g[1];if(b){var
v=ap(d,c,f,e,b,m);return cH(d,c,ap(d,c,f,e,bH(b),n),v)}var
w=ap(d,c,f,e,b,m);return er(ap(d,c,f,e,bH(b),n),w)}}function
hs(f,e,d){var
c=e,a=d;for(;;){if(c){var
g=c[2],h=c[1];if(a){var
i=a[2];if(b(f,h,a[1])){var
c=g,a=i;continue}return 0}return 0}return 1}}function
es(g,f,e,d,c,b,a){return hs(c,ap(g,f,e,d,1,b),a)}function
fy(d,c,a){return bH(b(d,c,a))}function
fz(f,e,c,a){var
d=b(e,c,a);return d?fy(f,c,a):d}function
ht(b,a){switch(b){case
0:return ne;case
1:return 1===a?nf:0===a?ng:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?nh:ni}}function
hu(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?nj:0;case
2:return 1===a?0:nk;default:return 1===a?0:0===a?nl:[0,a]}}function
et(c,b){return b?a(c,b[1]):0}function
fA(d,c,a){if(c){var
e=c[1];return a?b(d,e,a[1]):0}return 0}function
hv(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,aa(g,f,e,d,c,b,h),0]]:0}function
hw(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=ht(b[2],a[2]);return et(function(a){return[0,[0,aa(g,f,e,d,c,h,i),a]]},j)}function
cI(e,d,c,b,a){var
f=b[1],g=a[1],h=hu(b[2],a[2]);return et(function(a){return[0,[0,Z(e,d,c,f,g),a]]},h)}function
bu(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,hg(b[1],g,[0,[0,a],0])];case
1:return[0,[0,cF(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=bu(a,f,d,e,c,h,g,b[2]);return et(function(b){return hv(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=bu(a,f,d,e,c,h,g,b[2]),n=bu(a,f,d,e,c,h,g,l);return fA(function(b,g){return hw(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=bu(a,f,d,e,c,h,g,b[2]),q=bu(a,f,d,e,c,h,g,o);return fA(function(b,e){return cI(a,d,c,b,e)},q,p);default:var
i=b[1];return fz(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
cJ(a,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return fy(d,c,a);case
1:return b(d,c,a);case
2:return b(f,c,a);default:return fz(d,f,c,a)}}return 0}function
eu(c,i,h,g,b,a,f,e){var
d=bu(c,i,h,g,b,a,f,e);return d?cJ(c,b,a,d[1]):0}function
b7(g,f,e,d,c,b,a){return function(h){return ao(g,f,e,d,c,b,a,h)}}function
ab(e,d,c,b,a){return function(f,g){return ag(e,d,c,b,a,f,g)}}function
hx(c,b,a){return function(d,e){return Z(c,b,a,d,e)}}function
hy(g,l,f,k,e,d,c,j){var
m=j[3],n=j[2],o=j[1],h=a(b7(g,l,f,k,e,d,c),o),i=a(b7(g,l,f,k,e,d,c),m);switch(n){case
0:var
p=[0,[0,b(ab(g,f,e,d,c),i,h),2],0];return[0,[0,b(ab(g,f,e,d,c),h,i),2],p];case
1:return[0,[0,b(ab(g,f,e,d,c),h,i),0],0];case
2:return[0,[0,b(ab(g,f,e,d,c),h,i),2],0];case
3:return[0,[0,b(ab(g,f,e,d,c),i,h),2],0];case
4:return[0,[0,b(ab(g,f,e,d,c),h,i),3],0];default:return[0,[0,b(ab(g,f,e,d,c),i,h),3],0]}}function
fB(h,g,f,e,d,c,b,a){var
i=hy(h,g,f,e,d,c,b,a);return b5(function(a){return[0,a,0]},i)}function
hz(g,l,f,k,e,d,c,j){var
m=j[3],n=j[2],o=j[1],h=a(b7(g,l,f,k,e,d,c),o),i=a(b7(g,l,f,k,e,d,c),m);switch(n){case
0:return[0,[0,b(ab(g,f,e,d,c),h,i),0],0];case
1:var
p=[0,[0,b(ab(g,f,e,d,c),i,h),2],0];return[0,[0,b(ab(g,f,e,d,c),h,i),2],p];case
2:return[0,[0,b(ab(g,f,e,d,c),i,h),3],0];case
3:return[0,[0,b(ab(g,f,e,d,c),h,i),3],0];case
4:return[0,[0,b(ab(g,f,e,d,c),i,h),2],0];default:return[0,[0,b(ab(g,f,e,d,c),h,i),2],0]}}function
fC(h,g,f,e,d,c,b,a){var
i=hz(h,g,f,e,d,c,b,a);return b5(function(a){return[0,a,0]},i)}function
ev(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=b(A[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=ev(a(A[1],d),h);return[2,[4,ev(d,j),[6,[1,d],[0,i]]],k]}}function
nm(a){return ev(0,a)}function
aE(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1]];case
2:var
d=b[1],e=aE(c,b[2]);return[2,aE(c,d),e];case
3:var
f=b[1],g=aE(c,b[2]);return[3,aE(c,f),g];case
4:var
h=b[1],i=aE(c,b[2]);return[4,aE(c,h),i];case
5:return[5,aE(c,b[1])];default:var
j=b[2];return[6,aE(c,b[1]),j]}}function
hA(b,a){var
c=a[2],d=a[1],e=aE(b,a[3]);return[0,aE(b,d),c,e]}function
nn(q,h,f,g,c){if(typeof
c!=="number")switch(c[0]){case
1:var
m=c[1];if(0===m[0]){var
n=m[1];return b(g,q,n)?0:[5,b(f,n,n)]}return[1,m];case
3:var
a=c[2],d=c[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],s)],i]:c}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],r)],j]:c}return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c;case
5:var
e=d[1];if(typeof
a==="number")return 0;else
switch(a[0]){case
3:var
k=a[2],l=a[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,b(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,b(f,e,k[1])],l];return b(g,h,e)?a:[3,d,a];case
4:return[4,[3,[5,e],a[1]],[3,[5,e],a[2]]];case
5:return[5,b(f,e,a[1])];default:return b(g,h,e)?a:[3,d,a]}default:return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c}case
4:var
o=c[2],p=c[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return c}function
no(a){return a[1]}function
np(a){return a[2]}function
aF(c,a){var
d=b(u[8],a[1],[0,c[2]]);return aD(b(u[8],c[1],[0,a[2]]),d)}function
cK(c,a){var
d=b(u[8],a[1],[0,c[2]]),e=b(u[8],c[1],[0,a[2]]);return b(u[10],e,d)}function
aG(c,a){var
d=b(A[11],c[2],a[2]),e=b(u[8],a[1],[0,c[2]]),f=b(u[8],c[1],[0,a[2]]);return[0,b(u[5],f,e),d]}function
a9(c,a){var
d=b(A[11],c[2],a[2]);return[0,b(u[8],c[1],a[1]),d]}function
bM(b){var
c=b[2];return[0,a(u[6],b[1]),c]}function
b8(b,a){return aG(b,bM(a))}function
fD(b){var
a=b[1];return typeof
a==="number"?nq:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
fE(a,b){return el(a9,a,b)}function
nr(b,a){return typeof
a==="number"?ns:0===a[0]?fE(b,a[1]):fD(fE(b,a[1]))}function
nt(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
b9(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,b9(b,a[1],c)]:[1,b9(b,a[1],c),b,0]}function
fF(d,a,b,c){if(typeof
c==="number")return b9(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,b9(d,a[1],b)]:[1,b9(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,fF(d,a[1],b,e)]:[1,fF(d,a[1],b,f),h,e]}}var
nu=u[10],nv=u[8],nw=u[5],ny=0;function
nz(a,b){return eu(ny,nx,nw,nv,aD,nu,a,b)}var
ac=ab(0,u[5],u[7],u[6],aD),aH=hx(0,u[5],aD),cL=b7(0,nA,u[5],u[8],u[7],u[6],aD);function
hB(e){var
f=e[3],g=e[2],c=a(cL,e[1]),d=a(cL,f);switch(g){case
0:var
h=[0,[0,b(ac,d,b(aH,c,nB)),3],0];return[0,[0,b(ac,c,b(aH,d,nC)),3],h];case
1:return[0,[0,b(ac,c,d),0],0];case
2:return[0,[0,b(ac,c,b(aH,d,nD)),3],0];case
3:return[0,[0,b(ac,d,b(aH,c,nE)),3],0];case
4:return[0,[0,b(ac,c,d),3],0];default:return[0,[0,b(ac,d,c),3],0]}}function
hC(a){var
b=hB(a);return b5(function(a){return[0,a,0]},b)}function
hD(e){var
f=e[3],g=e[2],c=a(cL,e[1]),d=a(cL,f);switch(g){case
0:return[0,[0,b(ac,c,d),0],0];case
1:var
h=[0,[0,b(ac,d,b(aH,c,nF)),3],0];return[0,[0,b(ac,c,b(aH,d,nG)),3],h];case
2:return[0,[0,b(ac,d,c),3],0];case
3:return[0,[0,b(ac,c,d),3],0];case
4:return[0,[0,b(ac,d,b(aH,c,nH)),3],0];default:return[0,[0,b(ac,c,b(aH,d,nI)),3],0]}}function
hE(a){var
b=hD(a);return b5(function(a){return[0,a,0]},b)}var
nJ=u[10],nK=0;function
fG(a){return cJ(nK,aD,nJ,a)}var
nL=u[5],nM=0;function
hF(a,b){return cI(nM,nL,aD,a,b)}function
hG(e,d){var
a=b(u[17],e,d),c=a[1];return typeof
a[2]==="number"?c:b(u[5],c,nN)}function
fH(c,a){var
d=b(u[19],c,a);return b(u[13],d,nO)}function
cM(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=cM(a[1]),f=b[2],g=b[1],c=cM(e),h=c[2],i=c[1];return[0,fH(fH(g,f),i),h]}}function
cN(a,c){switch(a[0]){case
0:return[0,b(u[18],a[1],c)];case
1:var
d=a[1];return[1,d,cN(a[2],c)];default:var
e=a[2],f=a[1],g=cN(a[3],c);return[2,cN(f,c),e,g]}}function
ew(c){var
e=cM(c),f=e[2],d=e[1];if(b(u[12],d,0)){var
g=hG(a(u[6],f),d),h=a(u[6],g);return[0,cN(b6(u[7],c,f),d),h]}return[0,c,0]}function
ex(d){var
e=d[2],a=d[1];switch(e){case
0:var
f=cM(a),g=f[2],c=f[1];if(b(u[12],c,0))if(bH(aD(g,0)))if(bH(aD(b(u[19],c,g),c)))return 0;return[0,[0,ew(a),0]];case
1:return[0,[0,[0,a,0],e]];case
2:return[0,[0,ew(b6(u[7],a,nP)),3]];default:return[0,[0,ew(a),3]]}}function
hH(a){var
c=a[1],d=a[2];return[0,b(aH,c[1],[0,c[2]]),d]}function
hI(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
nQ=u[10],nR=u[8],nS=u[5],nU=0;function
cO(a,b){return bu(nU,nT,nS,nR,aD,nQ,a,b)}function
fI(a){return 0===a?1:3<=a?1:0}function
fJ(w,v){var
d=w,c=v;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
x=c[2],g=cO(d,c[1]);if(g){var
h=g[1];if(fG(h))return 1;var
d=[0,h,d],c=x;continue}return 0;case
1:var
y=c[2],i=cO(d,c[1]);if(i){var
j=ex(i[1]);if(j){var
d=[0,hH(j[1]),d],c=y;continue}return 1}return 0;default:var
z=c[3],A=c[2],k=cO(d,c[1]);if(k){var
B=k[1],l=cO(d,A);if(l){var
C=l[1],m=ex(B);if(m){var
n=m[1],o=n[1],p=o[1],D=n[2],E=o[2],q=ex(C);if(q){var
r=q[1],s=r[1],F=r[2],G=s[2],H=s[1];if(fI(D))if(fI(F))if(hI(b(aH,p,H))){var
f=z,e=a(u[6],E);for(;;){if(f){var
I=f[2],J=f[1],t=fJ([0,[0,b(ac,p,[0,e]),0],d],J);if(t){var
f=I,e=b(u[5],e,nV);continue}return t}return b(u[12],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
nW(b,a){return es(fG,hF,hC,hE,fJ,b,a)}function
hJ(a,b){return eu(nY,nX,aG,a9,aF,cK,a,b)}function
hK(a){return fB(n0,nZ,aG,a9,b8,bM,aF,a)}function
hL(a){return fC(n2,n1,aG,a9,b8,bM,aF,a)}function
hM(a){return cJ(n3,aF,cK,a)}function
hN(a,b){return cI(n4,aG,aF,a,b)}function
n5(b,a){return es(hM,hN,hK,hL,hJ,b,a)}function
aQ(a){if(typeof
a==="number")return 0===a?n6:n7;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aQ(a[2]);return aG(aQ(b),c);case
3:var
d=a[1],e=aQ(a[2]);return b8(aQ(d),e);case
4:var
f=a[1],g=aQ(a[2]);return a9(aQ(f),g);case
5:return fD(aQ(a[1]));default:return bM(aQ(a[1]))}}function
hO(a,b){return eu(n9,n8,aG,a9,aF,cK,a,b)}function
hP(a){return fB(n$,n_,aG,a9,b8,bM,aF,a)}function
hQ(a){return fC(ob,oa,aG,a9,b8,bM,aF,a)}function
hR(a){return cJ(oc,aF,cK,a)}function
hS(a,b){return cI(od,aG,aF,a,b)}var
t=[0,bH,ek,hb,fo,mI,A,mQ,el,hg,b5,fs,u,aD,nc,nd,fv,Y,hl,N,hm,hn,af,bt,b6,cB,cC,fw,fx,Z,ag,cD,cE,a7,aa,cF,ho,cG,hp,ao,a8,eo,ep,eq,hq,hr,cH,er,ap,hs,es,fy,fz,ht,hu,et,fA,hv,hw,cI,bu,cJ,eu,b7,ab,hx,hy,fB,hz,fC,ev,nm,aE,hA,nn,no,np,aF,cK,aG,a9,bM,b8,fD,fE,nr,nt,b9,fF,nz,ac,aH,cL,hB,hC,hD,hE,fG,hF,hG,fH,cM,cN,ew,ex,hH,hI,cO,fI,fJ,nW,hJ,hK,hL,hM,hN,n5,aQ,hO,hP,hQ,hR,hS,function(b,a){return es(hR,hS,hP,hQ,hO,a8(function(a){return hA(aQ,a)},b),a)}];a3(738,t,"Micromega_plugin.Micromega");var
fK=f[2],bN=f[7],of=f[3],S=a(bO[1],[0,$]),oe=0;function
og(a,c){function
d(c,b){return 1===b?e(m[1],a,oh,c):H(m[1],a,oi,c,b)}return b(S[11],d,c)}var
fL=S[1];function
hT(a){var
b=0;function
c(c,b,a){return a+b|0}return e(S[12],c,a,b)}function
oj(b,a){var
c=hT(b),d=hT(a);return c===d?e(S[9],$,b,a):$(c,d)}function
hU(a){return V(a,S[1])}function
ok(a){return e(S[4],a,1,S[1])}function
ol(a){try{var
b=1,c=function(c,b,a){if(1===a){if(1===b)return 0;throw O}throw O},d=1-e(S[12],c,a,b);return d}catch(a){a=w(a);if(a===O)return 0;throw a}}function
om(a){if(hU(a))return 0;try{var
b=function(c,b,f){var
d=b/2|0;if(0===(b%2|0))return e(S[4],c,d,a);throw O},c=[0,e(S[12],b,a,fL)];return c}catch(a){a=w(a);if(a===O)return 0;throw a}}function
cP(c,a){try{var
d=b(S[23],c,a);return d}catch(a){a=w(a);if(a===O)return 0;throw a}}function
on(b,a){var
c=cP(b,a)+1|0;return e(S[4],b,c,a)}function
hV(b,a){function
c(b,c,a){var
d=cP(b,a)+c|0;return e(S[4],b,d,a)}return e(S[12],c,b,a)}function
oo(d,c){var
b=fL,a=c;for(;;){if(0===a)return b;var
b=hV(b,d),a=a-1|0;continue}}function
op(c,a){var
f=h[7];function
g(e,d,a){var
f=J.caml_div(cP(e,c),d);return b(h[4],a,f)}var
d=e(S[12],g,a,f),i=S[1];function
j(c,g,b){var
f=g-e9(cP(c,a),d)|0;return 0===f?b:e(S[4],c,f,b)}return[0,e(S[12],j,c,i),d]}var
K=[0,fL,hU,ok,ol,cP,on,hV,oo,op,oj,og,S[12],om],T=a(bO[1],[0,K[10]]);function
oq(c,d){function
g(g,d){if(0===b(K[10],K[1],g)){var
h=a(f[40],d);return e(m[1],c,or,h)}var
i=K[11],j=a(f[40],d);return aO(m[1],c,os,j,i,g)}return b(T[11],g,d)}function
hW(c,a){try{var
d=b(T[23],c,a);return d}catch(a){a=w(a);if(a===O)return ot;throw a}}function
ou(b){var
c=T[1],d=a(K[3],b);return e(T[4],d,ov,c)}function
hX(a){return e(T[4],K[1],a,T[1])}function
hY(d,g,c){if(0===a(f[25],g))return c;var
h=b(fK,hW(d,c),g);return 0===a(f[25],h)?b(T[6],d,c):e(T[4],d,h,c)}function
hZ(g,c,d){if(0===a(f[25],c))return hX(ow);var
h=T[1];function
i(f,d,a){var
h=b(bN,c,d),i=b(K[7],g,f);return e(T[4],i,h,a)}return e(T[12],i,d,h)}function
h0(b,a){function
c(c,b,a){return hY(c,b,a)}return e(T[12],c,b,a)}function
ox(b,a){var
c=T[1];function
d(d,c,b){return h0(hZ(d,c,a),b)}return e(T[12],d,b,c)}function
oy(c){function
d(b){return a(f[3],b)}return b(T[24],d,c)}var
h1=T[12];function
oz(b){var
c=1;return e(h1,function(e,c,b){var
d=b?0===a(f[25],c)?1:0:b;return d},b,c)}var
oA=a(T[9],f[37]),h2=[0,hW,ou,hY,hX,hZ,ox,h0,oy,h1,oq,oA,oz,function(b){var
c=1;function
d(c,f,b){if(b){var
d=a(K[2],c);if(!d)return a(K[4],c);var
e=d}else
var
e=b;return e}return e(T[12],d,b,c)}];function
oB(k,j){var
c=k,a=j;for(;;){if(c){var
d=c[1],l=c[2],m=d[2],n=d[1];if(a){var
e=a[1],g=n===e[1]?1:0,o=a[2],p=e[2];if(g){var
h=b(f[26],m,p);if(h){var
c=l,a=o;continue}var
i=h}else
var
i=g;return i}return 0}return a?0:1}}function
oC(e){var
c=0,b=e;for(;;){if(b){var
d=b[1],g=b[2],h=d[1],i=[0,h,a(f[52],d[2])],c=c+a(br[20],i)|0,b=g;continue}return a(br[20],c)}}var
oD=0;function
oE(h,c){function
d(b){var
c=b[1],d=a(f[40],b[2]);return e(m[2],oF,d,c)}return b(g[11],d,c)}function
oG(a){function
d(i,h){var
c=i,a=h;for(;;){if(a){var
e=a[2],g=a[1];if(b(f[31],g,oH))return[0,[0,c,g],d(c+1|0,e)];var
c=c+1|0,a=e;continue}return 0}}return d(0,a)}function
oJ(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,ey,b(c+1|0,a)]}return 0}return b(0,a)}function
bP(d,c,a){return b(f[26],c,oK)?a:[0,[0,d,c],a]}function
h3(d,c,b){if(b){var
f=b[2],g=b[1],i=g[2],e=g[1],j=$(d,e)+1|0;if(2<j>>>0)return a(h[2],oL);switch(j){case
0:return bP(d,a(c,ey),b);case
1:return bP(e,a(c,i),f);default:return[0,[0,e,i],h3(d,c,f)]}}return bP(d,a(c,ey),0)}function
h4(d,c,b){if(b){var
f=b[2],g=b[1],e=g[1],i=$(d,e)+1|0,j=g[2];if(2<i>>>0)return a(h[2],oM);switch(i){case
0:return bP(d,c,b);case
1:return bP(e,c,f);default:return[0,[0,e,j],h4(d,c,f)]}}return bP(d,c,0)}function
oN(d){var
f=q[1];function
h(d,c){var
e=a(l[18],c[2]);return b(q[17],d,e)}var
c=e(g[16],h,f,d);return 0===b(q[23],c,q[1])?q[2]:c}function
oO(a,c){if(0===a[0]){var
d=a[1];if(0===d)return 0;if(1===d)return c}function
e(c){var
d=c[1];return[0,d,b(f[7],a,c[2])]}return b(g[13],e,c)}function
ez(o,n){var
c=o,a=n;for(;;){if(c){if(a){var
e=a[2],i=a[1],j=i[2],g=i[1],h=c[2],k=c[1],l=k[2],d=k[1];if(V(d,g)){var
m=b(f[1],l,j);if(b(f[26],m,oP)){var
c=h,a=e;continue}return[0,[0,d,m],ez(h,e)]}return gK(d,g)?[0,[0,d,l],ez(h,a)]:[0,[0,g,j],ez(c,e)]}return c}return a?a:0}}function
oQ(d,c){var
e=0,g=[0,function(a){return b(f[37],d[2],c[2])},e],h=[0,function(a){return $(d[1],c[1])},g];return a(l[31][1],h)}var
oR=a(l[31][2],oQ);function
h5(e,d){var
a=d;for(;;){if(a){var
b=a[1],c=$(b[1],e),f=a[2],g=b[2];if(-1===c){var
a=f;continue}return 0===c?[0,[0,g,a]]:0}return 0}}function
oS(c,b){var
a=h5(c,b);return a?[0,a[1][1]]:0}var
bQ=[0,oB,oC,oD,oE,oG,ey,oI,oJ,bP,h3,h4,oN,oO,ez,oR,h5,oS,function(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}];function
fM(a){return 0===a?oT:oU}function
oV(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fM(d);return D(m[1],c,oW,bQ[4],e,h,g)}function
oX(b,a){if(0===b){if(0===a)return 0}else
if(0!==a)return 1;return 1}function
oY(b,a){if(0!==b)if(0!==a)return 1;return 0}function
aI(d,c){if(typeof
c==="number")return b(m[1],d,oZ);else
switch(c[0]){case
0:return e(m[1],d,o0,c[1]);case
1:return e(m[1],d,o1,c[1]);case
2:var
f=a(q[33],c[1]);return e(m[1],d,o2,f);case
3:return b(m[1],d,o3);case
4:return H(m[1],d,o4,aI,c[2]);case
5:var
g=c[2],h=a(q[33],c[1]);return aO(m[1],d,o5,aI,g,h);case
6:return D(m[1],d,o6,aI,c[1],aI,c[2]);case
7:return D(m[1],d,o7,aI,c[1],aI,c[2]);default:return H(m[1],d,o8,aI,c[1])}}function
cQ(d,c){if(typeof
c==="number")return b(m[1],d,o9);else{if(0===c[0])return d9(m[1],d,o_,c[1],aI,c[2],cQ,c[3]);var
e=c[5],f=c[4],g=c[3],h=c[2],i=c[1],j=a(l[2],cQ);return Bq(m[1],d,o$,i,aI,h,bQ[4],g,aI,f,j,e)}}function
b_(e){var
a=e;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
8:var
d=a[1],c=1;break;case
0:case
1:return a[1];case
4:case
5:var
d=a[2],c=1;break;case
6:case
7:var
f=a[1],g=b_(a[2]),i=b_(f);return b(h[5],i,g);default:var
c=0}if(c){var
a=d;continue}return-1}}function
fN(a){if(typeof
a==="number")return-1;else{if(0===a[0]){var
c=a[2],d=a[1],f=fN(a[3]),i=b_(c),j=b(h[5],i,f);return b(h[5],d,j)}var
k=a[5],l=a[2],m=a[1],n=b_(a[4]),o=b_(l),p=b(h[5],o,n),q=b(h[5],m,p),r=function(c,a){var
d=fN(a);return b(h[5],c,d)};return e(g[16],r,q,k)}}function
a_(c,a){if(typeof
a!=="number")switch(a[0]){case
4:var
n=a[1],d=a_(c,a[2]);return[0,d[1],d[2],[4,n,d[3]]];case
5:var
e=a_(c,a[2]),f=e[2];return[0,[0,[0,f,e[3]],e[1]],f+1|0,[1,f]];case
6:var
o=a[2],g=a_(c,a[1]),p=g[3],q=g[1],i=a_(g[2],o),r=i[2],s=[6,p,i[3]];return[0,b(h[22],i[1],q),r,s];case
7:var
t=a[2],j=a_(c,a[1]),u=j[3],v=j[1],k=a_(j[2],t),w=k[2],x=[7,u,k[3]];return[0,b(h[22],k[1],v),w,x];case
8:var
l=a_(c,a[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[1,m]]}return[0,0,c,a]}function
eA(c,a){if(typeof
a!=="number"&&8===a[0]){var
b=a_(c,a[1]);return[0,b[1],b[2],[8,b[3]]]}return a_(c,a)}function
fO(b){var
a=b;for(;;){if(typeof
a!=="number"&&8===a[0]){var
a=a[1];continue}return a}}function
fP(f,o){var
c=o;for(;;)if(typeof
c==="number")return[0,f,0];else{if(0===c[0]){var
d=c[2],l=c[1];if(typeof
d!=="number"&&5===d[0])if(typeof
c[3]==="number"){var
c=[0,l,d[2],0];continue}var
p=c[3],i=eA(f,d),q=i[3],r=i[1],m=fP(i[2],p),s=m[1],t=[0,l,q,m[2]],u=function(b,a){return[0,a[1],[8,a[2]],b]};return[0,s,e(g[16],u,t,r)]}var
v=c[5],w=c[4],x=c[3],y=c[1],j=eA(f,fO(c[2])),z=j[3],A=j[2],B=j[1],k=eA(A,fO(w)),C=k[3],D=k[2],E=k[1],F=function(a){return fP(D,a)},G=b(g[13],F,v),n=a(g[39],G),H=n[2],I=n[1],J=b(h[22],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[8,a[2]],b]},M=e(g[16],L,K,J);return[0,e(g[16],h[5],0,I),M]}}function
pa(c,a){var
b=fP(c,a);if(l[1])aO(m[2],pb,cQ,a,cQ,b[2]);return b}function
h6(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[7,b,a];var
c=b}return c}function
fQ(c,d){var
e=a(q[22],c)+1|0;if(2<e>>>0)throw[0,F,pc];switch(e){case
0:return[4,[0,0,[1,c]],d];case
1:return 0;default:return b(q[24],c,q[2])?d:[6,[2,c],d]}}function
h7(c,b){var
d=c[2],e=c[1];return e?[4,[0,e,d],b]:fQ(a(l[18],d),b)}var
cR=a(bO[1],[0,K[10]]),cS=a(bO[1],[0,$]),cT=[0,cR[1]],cU=[0,cS[1]],eB=[0,0];function
pd(a){cT[1]=cR[1];cU[1]=cS[1];eB[1]=0;return 0}function
pe(c){try{var
a=b(cR[23],c,cT[1]);return a}catch(a){a=w(a);if(a===O){var
d=eB[1];cT[1]=e(cR[4],c,d,cT[1]);cU[1]=e(cS[4],d,c,cU[1]);eB[1]++;return d}throw a}}var
aR=[0,cR,cS,cT,cU,eB,pd,pe,function(a){return b(cS[23],a,cU[1])}];function
fR(a){var
c=a[2],d=a[1];function
e(b,a){return $(b[1],a[1])}return[0,b(g[41],e,d),c]}function
eC(c,b){var
d=b[2],e=a(aR[8],b[1]),g=K[11],h=a(f[40],d);return aO(m[1],c,pf,h,g,e)}function
eD(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fM(d),i=a(l[2],eC);return D(m[1],c,pg,i,e,h,g)}function
ph(c){function
d(d,c,b){var
e=b[1],f=b[2];return a(K[2],d)?[0,e,c]:[0,[0,[0,a(aR[7],d),c],e],f]}var
b=e(h2[9],d,c,pi);return fR([0,b[1],b[2]])}function
h8(c,d,n){var
e=n[2],j=n[1];if(a(K[2],d))var
q=b(bN,c,e),i=[0,b(bQ[13],c,j),q];else
if(0===a(f[25],c))var
i=pj;else{if(0===a(f[25],e))var
k=0;else
var
t=b(bN,c,e),k=[0,[0,a(aR[7],d),t],0];var
r=function(e){var
f=e[2],g=a(aR[8],e[1]),h=b(K[7],d,g),i=a(aR[7],h);return[0,i,b(bN,c,f)]},s=b(g[13],r,j),i=fR([0,b(h[22],k,s),pk])}var
o=i[2],p=i[1];if(l[1]){var
u=a(f[40],o),v=a(l[2],eC),w=a(f[40],e),x=a(l[2],eC),y=K[11],z=a(f[40],c);Br(m[2],pl,z,y,d,x,j,w,v,p,u)}return[0,p,o]}function
h9(c,b){return a(K[2],b)?[0,0,c]:[0,[0,[0,a(aR[7],b),c],0],pm]}function
h_(w,v,d,u){var
i=u[1],j=w[1],F=u[2],G=w[2];if(l[1]){var
H=a(f[40],d),I=a(aR[8],v);bX(m[2],pn,eD,j,K[11],I,H,eD,i)}var
J=i[2],x=a(aR[8],v);function
C(c,a){var
d=a[2],e=c[2],f=a[1],g=c[1];return gK(e,d)?-1:V(e,d)?b(K[10],g,f):1}var
p=[0,K[1],h[7]],o=[0,i,F];for(;;){var
q=o[2],c=o[1],_=p[2],$=p[1],U=c[1],W=[0,pp,K[1],0],X=function(c,d){var
e=c[3],f=c[2],j=d[2],k=c[1],l=a(aR[8],d[1]),g=b(K[9],l,x),h=g[2],i=g[1];return-1===C([0,f,e],[0,i,h])?[0,j,i,h]:[0,k,f,e]},n=e(g[16],X,W,U),D=n[3],Y=n[2],Z=n[1],E=0<D?[0,[0,Z,Y,D]]:0;if(E){var
r=E[1],s=r[3],t=r[2],aa=r[1];if(-1===C([0,t,s],[0,$,_])){var
k=a(f[15],d),y=b(bN,[0,-a(f[25],d)|0],aa),L=b(K[8],x,s-1|0),z=b(K[7],t,L),M=a(f[3],j[3]),A=h8(y,z,[0,j[1],M]),N=A[2],O=A[1],P=b(bN,k,c[3]),Q=b(fK,a(f[3],N),P),R=b(bQ[13],k,c[1]),B=[0,b(bQ[14],R,O),J,Q],S=fQ(a(l[18],k),q),T=h6(h7(h9(y,z),G),S);if(l[1])e(m[2],po,eD,B);var
p=[0,t,s],o=[0,B,T];continue}return[0,c,q]}return[0,c,q]}}var
p=[0,oe,fK,of,bN,K,h2,bQ,fM,oV,oX,oY,aI,cQ,b_,fN,eA,fO,pa,h6,fQ,h7,[0,aR,fR,eC,eD,ph,h8,h9,h_,function(c,a){var
d=a[1],f=a[2],e=b(bQ[17],c,d[1]);if(e){var
g=e[1];return function(a){return[0,h_([0,d,f],c,g,a)]}}return function(a){return 0}}]];a3(742,p,"Micromega_plugin.Polynomial");var
at=l[4],eE=l[5],pq=0,pr=0,ps=kb;function
h$(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(f[29],c[1],d[1])?[0,a]:0}return[0,a]}function
pt(a){var
c=a[2],d=b(at,f[3],a[1]);return[0,b(at,f[3],c),d]}function
pu(c,a){var
e=a[2],g=a[1],h=c[2],i=c[1];function
d(d,c,a){if(c){var
e=c[1];return a?[0,b(d,e,a[1])]:c}return a?a:0}var
j=d(f[39],h,e);return h$([0,d(f[38],i,g),j])}function
fS(c){var
d=c[1];if(d){var
e=c[2];if(e){var
g=e[1],h=a(f[24],d[1]),i=a(f[22],g),j=b(f[4],i,h);return[0,b(f[1],j,pv)]}}return 0}function
pw(e,d){var
a=fS(e),c=fS(d);return a?c?b(f[29],a[1],c[1]):1:0}var
b$=[0,h$,pt,pu,fS,pw,function(d,a){var
c=d[2],e=d[1];if(e){var
g=e[1];if(c){var
i=c[1],h=b(f[29],g,a);return h?b(f[29],a,i):h}return b(f[29],g,a)}return c?b(f[29],a,c[1]):1}],aS=a(eh[1],[0,$]),ia=p[7],ad=a(br[18],[0,ia[1],ia[2]]),ib=[0,h[7]],ca=[a5,py,a4(0)],px=0;function
pz(a){function
d(h,g){var
a=h,c=g;for(;;){switch(a[0]){case
0:return b(aS[4],a[1],c);case
1:var
f=a[3],e=a[2];break;default:var
f=a[2],e=a[1]}var
a=e,c=d(f,c);continue}}return d(a,aS[1])}function
cV(b,a){switch(a[0]){case
0:return e(m[1],b,pA,a[1]);case
1:return d9(m[1],b,pB,a[1],cV,a[2],cV,a[3]);default:return D(m[1],b,pC,cV,a[1],cV,a[2])}}function
fT(d,c){if(c){var
e=a(f[40],c[1]);return b(h[49],d,e)}return b(h[49],d,pD)}function
ic(b,a){return D(m[1],b,pE,fT,a[1],fT,a[2])}function
pF(a,c){b(h[49],a,pG);var
d=0;function
f(b,c){return e(m[1],a,pH,b)}e(aS[15],f,c,d);return b(h[49],a,pI)}function
pJ(a,c){b(h[49],a,pK);var
d=0;function
f(b,c){return e(m[1],a,pL,b)}e(aS[15],f,c,d);return b(h[49],a,pM)}function
pN(b,a){return ic(b,a[1])}function
id(c,d){var
g=d[2],i=g[2],j=g[1],k=d[1];if(j){var
l=a(f[40],j[1]);e(m[1],c,pO,l)}b(p[7][4],c,k);if(i){var
n=a(f[40],i[1]);return e(m[1],c,pP,n)}return b(h[49],c,pQ)}function
pR(c,a){function
d(b,a){return id(c,[0,b,a[1][1]])}return b(ad[11],d,a)}function
pS(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=p[7][4],i=a(f[40],e);return D(m[1],c,pT,i,h,d,g)}function
ie(c,a){var
d=c[4],e=c[3],g=a[4],h=a[2],i=a[1],j=c[2],k=c[1];if(e===a[3])if(d===g){var
f=b(b$[3],k,i);return f?[0,[0,f[1],[2,j,h],e,d]]:0}throw[0,F,pU]}function
pV(f,c,d){try{var
a=b(ad[7],d,f),g=ie(c,a[1]);if(g){a[1]=g[1];var
h=0;return h}throw[0,ca,[2,c[2],a[1][2]]]}catch(a){a=w(a);if(a===O)return e(ad[9],d,f,[0,c]);throw a}}var
ig=[a5,pW,a4(0)];function
eF(d,c,b){var
e=ib[1];if(a(ad[14],b)<e)return pV(d,c,b);throw ig}function
eG(d,c){var
k=a(b$[1],c[1]);if(k){var
l=k[1],i=l[2],j=l[1];if(d){var
e=d[1][2],h=function(a){return b(f[9],a,e)};if(1===a(f[25],e))var
o=c[4],p=c[3],q=c[2],r=b(at,h,i),m=[0,[0,b(at,h,j),r],q,p,o];else
var
t=c[3],u=c[4],v=c[2],w=b(at,h,j),m=[0,[0,b(at,h,i),w],v,u,t];if(b(f[31],e,pX))var
s=function(a){var
c=a[1];return[0,c,b(f[9],a[2],e)]},n=b(g[13],s,d);else
var
n=d;return[0,n,m]}return b(b$[6],[0,j,i],pY)?1:0}return 0}function
pZ(a){return 0===a?f[26]:f[30]}function
fU(h){var
d=0,c=0,b=h;for(;;){if(b){var
e=b[2],g=a(f[25],b[1][2]);if(0===g)throw[0,F,p0];if(1===g){var
c=c+1|0,b=e;continue}var
d=d+1|0,b=e;continue}return[0,d,c]}}function
fV(a,e){var
b=a[3],c=a[1],f=a[2],d=fU(c),g=d[2],h=d[1],i=[0,e],j=0===f?[0,[0,b],[0,b]]:[0,[0,b],0];return eG(c,[0,j,i,g,h])}function
ih(d){var
c=a(ad[1],1e3);function
f(b,a){return[0,b,a]}var
h=b(l[23],f,d),i=aS[1];function
j(f,d){var
h=d[2],i=d[1],a=fV(i,h);if(typeof
a==="number"){if(0===a)throw[0,ca,[0,h]];return f}eF(a[1],a[2],c);var
j=i[1];function
k(c,a){return b(aS[4],a[1],c)}return e(g[16],k,f,j)}return[0,c,e(g[16],j,i,h)]}function
fW(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return e(ad[13],d,b,c)}function
eH(e,c){var
h=c[2],i=e[2],j=c[1],k=e[1];if(b(f[31],i,p1))if(b(f[31],h,p2)){var
d=function(s,r){var
c=s,a=r;for(;;){if(c){if(a){var
j=a[2],m=a[1],n=m[2],k=m[1],l=c[2],o=c[1],p=o[2],e=o[1];if(e===k){var
t=b(f[9],n,h),u=b(f[9],p,i),q=b(f[1],u,t);if(b(f[26],q,p3)){var
c=l,a=j;continue}return[0,[0,e,q],d(l,j)]}if(e<k){var
v=d(l,a);return[0,[0,e,b(f[9],p,i)],v]}var
w=d(c,j);return[0,[0,k,b(f[9],n,h)],w]}var
x=function(a){var
c=a[1];return[0,c,b(f[9],a[2],i)]};return b(g[13],x,c)}if(a){var
y=function(a){var
c=a[1];return[0,c,b(f[9],a[2],h)]};return b(g[13],y,a)}return 0}},a=d(k,j);return[0,a,fU(a)]}throw[0,F,p4]}function
ii(q,g,c,e){var
h=e[3],i=e[2],j=e[1],k=b(p[7][17],q,g);if(k){var
l=k[1],d=function(b,a){return a?[0,[0,l,g,[0,[0,[0,a[1]],0],c[2],c[3],c[4]]],b]:b},m=c[1],n=m[2],o=m[1];if(1===a(f[25],l)){var
r=d(h,n);return[0,d(j,o),i,r]}var
s=d(h,o);return[0,d(j,n),i,s]}return[0,j,[0,[0,g,c],i],h]}function
ij(d,c){var
j=c[1];function
k(c,b,a){return ii(d,c,b[1],a)}var
h=e(ad[13],k,j,p5),l=h[3],m=h[2],n=h[1],o=a(ad[14],c[1]),i=a(ad[1],o);function
p(a){return e(ad[9],i,a[1],[0,a[2]])}b(g[11],p,m);function
q(e){function
c(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=a(eE,k[1][1]),t=a(eE,r[1]),u=a(f[3],j),v=b(f[9],t,u),w=b(f[9],s,l),x=b(f[1],w,v),m=eH([0,q,l],[0,p,a(f[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,d,k[2],h[2]],n[2],n[1]],c=eG(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,ca,o[2]];return 0}return eF(c[1],c[2],i)}return b(g[11],c,l)}b(g[11],q,n);return[0,i,b(aS[6],d,c[2])]}function
ik(h,g,t,s,r,e){var
c=e[2],d=e[1],i=b(p[7][17],h,d);if(i){var
j=i[1],k=b(f[30],j,p6)?a(f[3],g):g,l=a(f[15],j),m=eH([0,t,k],[0,d,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=b(f[9],s,k),o=function(a){var
c=b(f[9],a,l);return b(f[1],x,c)},q=c[1],y=q[1],z=b(at,o,q[2]),A=[0,b(at,o,y),z];return[0,w,[0,A,[1,h,r,c[2]],u,v]]}return[0,d,c]}function
il(d,e,h,g,c){var
i=a(eE,b(p[7][17],d,e)),j=a(ad[14],c[1]),f=a(ad[1],j),k=c[1];function
l(k,j){var
c=ik(d,i,e,h,g,[0,k,j[1]]),b=c[2],a=eG(c[1],b);if(typeof
a==="number"){if(0===a)throw[0,ca,b[2]];return 0}return eF(a[1],a[2],f)}b(ad[11],l,k);return[0,f,b(aS[6],d,c[2])]}function
p7(a){var
b=0;function
c(c,a,b){return(b+a[1][4]|0)+a[1][3]|0}return e(ad[13],c,a,b)}var
bR=a(bO[1],[0,$]);function
p8(c,b){var
d=0;function
g(d,b,g){var
e=a(f[40],b);return H(m[1],c,p9,d,e)}return e(bR[12],g,b,d)}function
im(k,a){function
d(m,e,l){var
c=m,a=l;for(;;){if(c){var
g=c[2],h=c[1],i=h[2],j=h[1];try{var
n=b(bR[23],j,k),o=b(f[6],n,i),p=d(g,b(f[1],e,o),a);return p}catch(b){b=w(b);if(b===O){var
c=g,a=[0,[0,j,i],a];continue}throw b}}return[0,e,a]}}return d(a,p_,0)}function
io(g,e,d){function
c(a){var
c=b(f[4],a,e);return b(f[9],c,g)}var
i=d[2],j=d[1],k=a(f[25],g);if(0===k)return b(b$[6],d,e)?p$:a(h[2],qa);if(1===k){var
l=b(at,c,i);return[0,b(at,c,j),l]}var
m=b(at,c,j);return[0,b(at,c,i),m]}function
fX(g,f,c){function
d(k,j,i){var
c=im(g,k),l=c[1],d=b(p[7][17],f,c[2]),m=d?d[1]:qd,n=io(m,l,j[1][1]),e=b(b$[3],i,n);return e?e[1]:a(h[2],qc)}return e(ad[13],d,c,qb)}function
ip(d){var
e=d[1];if(e){var
g=d[2],c=e[1];if(g){var
h=g[1];if(b(f[29],c,qe))if(b(f[29],qf,h))return qg;var
k=a(f[22],h),l=a(f[24],c);return b(f[29],l,k)?a(f[24],c):c}return b(f[29],c,qh)?qi:a(f[24],c)}var
i=d[2];if(i){var
j=i[1],m=a(f[22],j);return b(f[29],qj,m)?qk:a(f[22],j)}return ql}function
iq(h,l,k,d,c){function
e(c,f){var
m=a(l,c);try{var
q=function(a){return a[1][1]!==h?1:0},d=b(g[29],q,m)[1],j=d[1],r=e(il(j,d[2],d[3],d[4],c),[0,[0,j,c],f]);return r}catch(d){d=w(d);if(d===O){var
n=a(k,c);try{var
o=function(a){return a[1]!==h?1:0},i=b(g[29],o,n)[1],p=e(ij(i,c),[0,[0,i,c],f]);return p}catch(a){a=w(a);if(a===O)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
fY(d,c,b,a){try{var
e=iq(d,c,b,ih(a),0);return e}catch(a){a=w(a);if(a[1]===ca)return[1,a[2]];throw a}}function
ir(w,v){var
d=v,c=0,i=0,j=0,h=0;for(;;){if(d){var
k=d[2],n=d[1],b=n[2],l=n[1];if(l){var
o=l[2],p=l[1],x=p[2];if(w===p[1]){var
m=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(b),q=b[1],r=q[2],s=q[1];if(1===a(f[25],x)){var
y=m(h,r),d=k,c=[0,[0,o,b],c],i=m(i,s),h=y;continue}var
z=m(h,s),d=k,c=[0,[0,o,b],c],i=m(i,r),h=z;continue}var
d=k,c=[0,[0,l,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
d=k,c=[0,[0,0,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
t=a(g[1],i),A=0,B=function(b,a){return b+a|0},C=e(g[16],B,A,i),u=a(g[1],h),D=0,E=function(b,a){return b+a|0};return[0,c,j+u*C+t*e(g[16],E,D,h)-u*t]}}var
fZ=[0,ir,function(a){var
c=a[2],d=[0,0,fW(a)];function
f(b,a){var
d=a[1],c=ir(b,a[2]);return[0,[0,[0,b,c[2]],d],c[1]]}var
h=e(aS[15],f,c,d)[1];function
i(b,a){return kb(b[2],a[2])}return b(g[41],i,h)}];function
f0(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(f[26],c[1],d[1])}return 0}function
qm(a,h){var
c=a[1];if(c){var
d=a[2];if(d){var
e=d[1],g=b(f[26],c[1],e);return g?b(f[26],h,e):g}}return 0}function
f1(b,e){var
a=e;for(;;){if(a){var
c=a[2],d=a[1][1];if(d===b)return[0,1,c];if(d<b){var
a=c;continue}return[0,0,a]}return qn}}function
is(d){var
a=d;for(;;){if(a){var
b=a[1],c=b[1],e=a[2],f=b[4],g=b[3],h=b[2];if(c)if(!c[2])return[0,[0,c[1][1],c,h,g,f]];var
a=e;continue}return 0}}function
it(f,k){var
j=is(f);if(j)return[0,j[1]];var
b=f;a:for(;;){if(b){var
c=b[1],i=c[1],a=i,o=b[2],p=c[4],q=c[3],r=c[2];for(;;){if(a){var
h=a[1][1],n=a[2],l=0,m=function(d){return function(a,b){var
c=b[2];return f1(d,b[1])[1]?f0(c[1])?a+1|0:a:a}}(h);if(2!==e(g[16],m,l,k)){var
a=n;continue}var
d=[0,h]}else
var
d=0;if(d)return[0,[0,d[1],i,r,q,p]];var
b=o;continue a}}return 0}}var
f2=[0,f0,qm,f1,is,it,function(i){var
h=fW(i),j=0;function
k(c,d){var
a=d[2],e=a[1],g=e[1],j=d[1];if(g){var
h=e[2];if(h){var
i=g[1];return b(f[26],i,h[1])?[0,[0,j,i,a[2],a[4]+a[3]|0],c]:c}}return c}var
c=e(g[16],k,j,h),d=it(c,h);if(d){var
a=d[1];return[0,[0,[0,a[1],a[2],a[3],a[4]],0],0]}var
l=0;function
m(s,f){var
p=f[1],e=p,n=h,k=s,t=f[4],u=f[3],v=f[2];a:for(;;){if(e){var
o=e[1][1],c=n,b=0,a=0,q=e[2],r=t-1|0;for(;;){if(c){var
g=c[2],l=c[1],d=l[2],i=d[3]+d[4]|0,m=f1(o,l[1]),j=m[2];if(0===m[1]){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}if(f0(d[1])){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}var
c=g,b=(b+i|0)+r|0,a=[0,[0,j,d],a];continue}var
e=q,n=a,k=[0,[0,[0,o,p,v,u],b],k];continue a}}return k}}var
n=e(g[16],m,l,c);function
o(b,a){return $(b[2],a[2])}return b(g[41],o,n)}];function
qo(i,d){var
j=0;function
k(d,c){var
e=a(p[7][18],c[1]);return b(h[5],d,e)}var
c=e(g[16],k,j,d),l=[0,[0,e(p[7][11],c,qq,i),0,qp],d],f=fY(c,f2[6],fZ[2],l);if(0===f[0]){var
n=f[1][1];try{var
q=[0,fX(bR[1],c,n[1])];return q}catch(c){c=w(c);if(a(bv[21],c)){var
o=a(ei[1],c);b(m[2],qr,o);return 0}throw c}}return 0}var
qs=[0,qo,function(j){var
d=fY(h[7],f2[6],fZ[2],j);if(0===d[0]){var
c=d[1][2],b=bR[1];for(;;){if(c){var
f=c[1],i=f[1],k=c[2],l=ip(fX(b,i,f[2][1])),c=k,b=e(bR[4],i,l,b);continue}var
m=0,n=function(c,b,a){return[0,[0,c,b],a]},o=e(bR[12],n,b,m);return[0,a(g[6],o)]}}return[1,d[1]]}];function
bw(b,a){return eH(b,a)[1]}function
iu(d,c,a){var
f=0;function
h(c,f){function
h(a,e){var
c=b(d,f,e);return c?[0,c[1],a]:a}return e(g[16],h,c,a)}return e(g[16],h,f,c)}function
eI(b,a){if(0===b)if(0===a)return 0;return 1}function
f3(s,r,q){var
j=q[2],k=q[1],l=r[2],m=r[1],n=j[3],e=j[2],g=j[1],o=l[3],h=l[2],i=l[1],t=b(p[7][17],s,i),u=b(p[7][17],s,g);if(t)if(u){var
c=u[1],d=t[1],v=a(f[25],c);if(-1===e9(a(f[25],d),v)){var
w=a(f[15],c),x=b(f[9],n,w),y=a(f[15],d),z=b(f[9],o,y),A=b(f[1],z,x),B=eI(h,e),C=[0,g,a(f[15],c)],D=[0,bw([0,i,a(f[15],d)],C),B,A],E=[0,k,a(f[15],c)];return[0,[0,bw([0,m,a(f[15],d)],E),D]]}if(0===h){var
F=b(f[9],n,qt),G=b(f[9],d,c),H=a(f[3],G),I=b(f[9],o,H),J=b(f[1],I,F),K=eI(h,e),L=b(f[9],d,c),M=[0,bw([0,i,a(f[3],L)],[0,g,qu]),K,J],N=b(f[9],d,c);return[0,[0,bw([0,m,a(f[3],N)],[0,k,qv]),M]]}if(0===e){var
O=b(f[9],o,qw),P=b(f[9],c,d),Q=a(f[3],P),R=b(f[9],n,Q),S=b(f[1],R,O),T=eI(h,e),U=b(f[9],c,d),V=[0,bw([0,g,a(f[3],U)],[0,i,qx]),T,S],W=b(f[9],c,d);return[0,[0,bw([0,k,a(f[3],W)],[0,m,qy]),V]]}return 0}return 0}function
iv(a){function
b(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=fV(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b}return e(g[16],b,qz,a)}function
iw(t,e,s){var
k=e[2],l=e[1],m=e[4][1],u=m[2],v=m[1];function
n(e,c,a){if(c){var
f=c[1][3];if(a){var
d=a[1];return b(e,f,d)?[0,[0,l,k,d]]:c}return c}return a?[0,[0,l,k,a[1]]]:0}var
c=n(f[29],t,v),d=n(f[30],s,u);if(c)if(d){var
g=d[1],i=g[2],o=g[1],j=c[1],p=j[1],w=j[2];if(b(f[29],j[3],g[3]))return[0,[0,c,d]];var
q=i[1];if(q){var
r=f3(q[1][1],[0,p,w],[0,o,i]);return r?[1,r[1]]:a(h[2],qA)}return[1,[0,bw([0,p,qC],[0,o,qB]),i]]}return[0,[0,c,d]]}var
C=[0,pq,at,eE,pr,ps,b$,aS,px,ad,ib,ca,pz,cV,fT,ic,pF,pJ,pN,id,pR,pS,ie,ig,eF,eG,pZ,fU,fV,ih,fW,eH,ii,ij,ik,il,p7,bR,p8,im,io,fX,ip,iq,fY,fZ,f2,qs,[0,bw,iu,eI,f3,iv,iw,function(t,a){function
c(a){switch(a[0]){case
0:var
j=a[1];return[0,[0,[0,[0,j,qD],0],b(g[5],t,j)],0];case
1:var
u=a[3],v=a[1],w=c(a[2]),x=c(u);return iu(function(a,b){return f3(v,a,b)},w,x);default:var
y=a[2],z=c(a[1]),A=c(y),f=iv(b(h[22],z,A));if(0===f[0]){var
B=f[1],C=function(a,c){if(0===a[0]){var
b=a[1];return iw(b[1],c,b[2])}return a},i=e(g[16],C,qE,B);if(0===i[0]){var
k=i[1],d=k[2],l=k[1];if(l){var
m=l[1],n=m[2],o=m[1];if(d){var
p=d[1];return[0,[0,o,n],[0,[0,p[1],p[2]],0]]}var
r=n,q=o}else{if(!d)return 0;var
s=d[1],r=s[2],q=s[1]}return[0,[0,q,r],0]}return[0,i[1],0]}return[0,f[1],0]}}return c(a)}]];a3(744,C,"Micromega_plugin.Mfourier");var
qF=0,qG=0,qH=0,qI=t[13],qJ=t[12][8],qL=0;function
qM(b){return[1,a(l[29][7],b)]}var
cW=[0,l[30][7],qM,qL,qK,qJ,qI],qN=t[77],qO=t[80],qR=l[29][9],bx=[0,function(b){return[0,a(l[30][7],b),0]},qR,qQ,qP,qO,qN];function
cX(e,c){function
d(c){switch(c[0]){case
0:var
g=a(e[2],c[1]);return a(p[6][4],g);case
1:var
h=a(l[29][2],c[1]);return a(p[6][2],h);case
2:var
i=c[1],j=d(c[2]),k=d(i);return b(p[6][7],k,j);case
3:var
m=c[1],n=d(c[2]),o=a(p[6][8],n),q=d(m);return b(p[6][7],q,o);case
4:var
r=c[2],s=d(c[1]),t=d(r);return b(p[6][6],s,t);case
5:var
u=d(c[1]);return a(p[6][8],u);default:var
v=c[2],w=d(c[1]),x=a(l[29][3],v),f=function(c){if(0===c){var
d=a(e[2],e[4]);return a(p[6][4],d)}var
g=f(c-1|0);return b(p[6][6],w,g)};return f(x)}}return d(c)}function
ix(b){function
c(f,c,b){var
d=a(l[30][2],f),e=1===c?[1,d]:[6,[1,d],a(l[30][3],c)];return V(b,qT)?e:[4,e,b]}return e(p[5][12],c,b,qS)}function
iy(o,h){function
p(c){var
d=a(f[24],c);return b(f[26],d,c)}if(b(g[23],p,h)){var
i=function(a){return ix(b(g[5],o,a))},e=qX,c=0,d=h;for(;;){if(d){var
j=d[2],k=d[1];if(b(f[26],k,qU)){var
c=c+1|0,d=j;continue}var
q=a(l[18],k),m=[0,a(l[30][7],q)],n=V(m,qV)?i(c):[4,m,i(c)],r=V(e,qW)?n:[2,n,e],e=r,c=c+1|0,d=j;continue}return e}}throw[0,F,qY]}function
iz(e,d){var
b=d;for(;;){var
c=a(e,b);if(V(c,b))return c;var
b=c;continue}}function
iA(b,e){var
d=H(t[74],b[3],b[4],b[5],b[6]);function
c(b){if(typeof
b!=="number")switch(b[0]){case
3:var
e=b[1],f=c(b[2]);return a(d,[3,c(e),f]);case
4:var
g=b[1],h=c(b[2]);return a(d,[4,c(g),h])}return a(d,b)}return c(e)}function
eJ(b,a){return iz(function(a){return iA(b,a)},a)}function
iB(d){function
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
h=c[1],k=a[2],l=c[2];if(V(h,a[1]))var
e=[0,[2,h,[4,l,k]]],b=1;else
var
e=0,b=1}else
var
b=0;break;case
3:if(typeof
a==="number")var
b=0;else
if(3===a[0]){var
i=c[1],m=a[2],n=c[2];if(V(i,a[1]))var
e=[0,[3,i,[4,n,m]]],b=1;else
var
e=0,b=1}else
var
b=0;break;default:var
b=0}if(!b)var
e=0;if(e){var
f=g,d=[0,e[1]];continue}return[4,c,j(g,[0,a])]}var
f=g,d=[0,a];continue}return d?d[1]:0}}var
a=d,c=0,f=0;for(;;){if(typeof
a!=="number"&&4===a[0]){var
e=[0,a[1],c],a=a[2],c=e;continue}return j(b(g[41],J.caml_compare,[0,a,c]),f)}}function
iC(c,f){var
h=0;function
i(d,a){return[0,b(p[6][1],c,a),d]}var
d=e(g[16],i,h,f);if(V(c,p[5][1])){var
j=[1,q[1]],k=a(g[6],d);return[0,a(p[7][5],[0,[1,q[2]],k]),0,j]}var
l=[1,q[1]],m=a(g[6],d);return[0,a(p[7][5],[0,[1,q[1]],m]),0,l]}function
iD(a){function
c(f,d){var
b=f,a=d;for(;;){if(a){if(0===a[1][2]){var
b=b+1|0,a=a[2];continue}var
g=c(b+1|0,a[2]),h=1,i=p[7][3],j=function(a){return q0};return[0,[0,e(p[7][10],b+1|0,j,i),h,qZ],g]}return 0}}return c(0,a)}function
q1(a){switch(a){case
0:return q2;case
1:return q3;case
2:return q4;default:return q5}}var
eK=a(eh[1],[0,p[5][10]]);function
iE(c){function
f(a){return a[1]}var
d=b(g[13],f,c),i=a(eK[5],p[5][1]);function
j(c,a){function
d(c,d,a){return b(eK[4],c,a)}return e(p[6][9],d,a,c)}var
k=e(g[16],j,i,d),l=0;function
m(b,a){return[0,iC(b,d),a]}var
n=e(eK[15],m,k,l),o=[1,q[2]],r=1;function
s(a){return 2===a[2]?[1,q[2]]:[1,q[1]]}var
t=b(g[13],s,c),u=[0,a(p[7][5],[0,[1,q[2]],t]),r,o],v=[0,u,iD(c)],w=b(h[22],v,n),x=[1,q[1]];return[0,[0,a(p[7][5],[0,[1,q[2]],0]),1,x],w]}var
q6=l[30][7];function
f4(d,c){var
e=c[1],g=d[1],f=c[2];if(e){var
j=e[2],i=function(d,c){if(d){var
e=d[1],m=d[2];if(c){var
j=c[1],f=i(m,c[2]),k=b(q[23],e,q[1]);if(-1===k){var
n=[0,a(l[30][1],j)];return[4,[2,[0,a(g,e)],n],f]}if(0===k)return f;var
o=[0,a(l[30][1],j)];return[4,[3,[5,a(g,e)],o],f]}return a(h[2],q7)}return 0};return iB(eJ(d,i(j,f)))}return a(h[2],q8)}var
q_=[a5,q9,a4(0)],f5=[a5,q$,a4(0)],aT=a(bO[1],[0,p[5][10]]);function
iF(c){var
d=[0,0];function
o(b,a){return $(b[1],a[1])}var
h=[0,aT[1],0];function
i(i,h){var
j=i[2],k=i[1],q=h[2],m=[0,h[1],0];function
n(g,j,i){var
h=i[2],c=i[1];if(V(g,p[5][1]))return[0,c,h];try{var
q=b(aT[23],g,c),l=q,k=c}catch(a){a=w(a);if(a!==O)throw a;var
m=e(aT[4],g,d[1],c),n=d[1];d[1]++;var
l=n,k=m}var
o=0===a(f[25],j)?h:[0,[0,l,j],h];return[0,k,o]}var
c=e(p[6][9],n,k,m),r=c[2],s=c[1],t=b(p[6][1],p[5][1],k),u=a(f[3],t);if(0===j)var
l=0;else{if(!(3<=j))throw f5;var
l=1}return[0,s,[0,[0,b(g[41],o,r),l,u],q]]}return e(g[17],i,c,h)[2]}function
iG(e){var
f=iE(e);try{var
c=a(C[47][2],f);if(0===c[0])var
i=a(p[7][8],c[1]),d=[0,a(l[22],i)];else
var
d=0;return d}catch(c){c=w(c);if(a(bv[21],c)){if(C[4]){var
g=a(ei[1],c);b(m[2],ra,g);a(h[46],h[24])}return 0}throw c}}function
iH(c){try{var
d=iF(c),f=a(C[47][2],d);if(0===f[0])var
h=0;else{var
i=f[1];if(C[4])e(m[2],rb,C[13],i);var
k=b(C[48][7],d,i),n=a(g[3],k)[1],o=function(a){return[0,a[1]+1|0,a[2]]},j=b(g[13],o,n);if(C[4])e(m[2],rc,p[7][4],j);var
q=a(p[7][8],j),h=[0,a(l[22],q)]}return h}catch(a){a=w(a);if(a===f5)return iG(c);throw a}}function
iI(d){var
b=a(g[39],d),e=b[2],c=iH(b[1]);return c?[0,[0,c[1],e]]:0}function
rd(e,c){var
d=a(g[1],c)-1|0,f=b(l[14],0,d),i=b(g[40],c,f);function
j(a){return 1===a[1][2]?1:0}var
k=b(g[32],j,i)[2];function
m(b){var
c=b[1],d=c[2],f=b[2],g=c[1];return 1===d?a(h[2],re):[0,[0,cX(e,g),d],f]}return iI(b(g[13],m,k))}function
iJ(c,b){try{var
e=rd(c,b);return e}catch(b){b=w(b);if(a(bv[21],b)){var
d=a(ei[1],b);a(h[27],d);return 0}throw b}}function
cY(e,c){var
d=a(g[1],e),f=b(h[5],c,e9(d,c));return b(h[5],d,f)}function
rf(d,b,a){var
e=cY(a,d);C[10][1]=e;var
c=iJ(b,a);return c?[0,f4(b,c[1])]:0}function
rg(n,c){a(p[22][1][6],0);var
o=cY(c,n);C[10][1]=o;function
q(c,b){return[0,c,[0,a(l[30][1],b)]]}var
f=b(l[23],q,c);function
r(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[3,c[2],a[2]];return[0,[0,[4,g,f],b(t[53],e[2],d[2])],h]}var
s=b(l[9],r,f),u=0;function
v(b,a){var
c=a[1],d=c[2],e=a[2],f=c[1];return d?[0,[0,[0,f,d[1]],e],b]:b}var
w=e(g[16],v,u,s),i=b(h[22],f,w);function
x(a){return cX(bx,a[1][1])}var
y=b(g[13],x,i),z=aT[1];function
A(c,b){function
d(c,f,b){var
d=a(p[5][13],c);return d?e(aT[4],d[1],c,b):b}return e(p[6][9],d,b,c)}var
B=e(g[16],A,z,y);function
j(b){var
c=[0,bx[4]];function
d(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}return e(p[5][12],d,b,c)}var
D=d9(t[63],bx[3],bx[4],t[79],t[80],t[82],t[81],t[77]),E=0;function
F(d,c,b){var
e=[1,a(D,j(d))];return[0,[0,[0,j(c),3],e],b]}var
G=e(aT[12],F,B,E),k=b(h[22],i,G);function
H(a){return a[1]}var
m=iJ(bx,b(g[13],H,k));if(m){var
I=f4(bx,m[1]),d=function(c){if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
e=a(l[29][1],c[1]);return b(g[5],k,e)[2];case
1:return[1,c[1]];case
2:var
f=c[1];return[2,f,d(c[2])];case
3:var
h=c[1],i=d(c[2]);return[3,d(h),i];case
4:var
j=c[1],m=d(c[2]);return[4,d(j),m];default:return[5,c[1]]}};return[0,d(I)]}return 0}function
rh(c){function
h(a){return a[1]}var
i=b(g[13],h,c),j=a(p[6][4],ri);function
k(c,a){return b(p[6][7],a,c)}var
l=e(g[16],k,j,i),m=0;function
n(b,c,a){return V(b,p[5][1])?a:[0,b,a]}var
d=e(p[6][9],n,l,m);function
o(c){var
e=c[1],h=c[2],i=b(p[6][1],p[5][1],e),j=a(f[3],i);function
k(a){return b(p[6][1],a,e)}var
l=b(g[13],k,d);return[0,a(p[7][5],l),h,j]}return[0,b(g[13],o,c),d]}function
rj(b,a){return[2,b,a]}function
rk(b,a){return[4,b,a]}function
rl(a){return[0,a]}function
rm(a){return[5,a]}function
rn(e,d,c){var
a=c;for(;;){if(a){var
f=a[2];if(b(e,d,a[1]))return 1;var
a=f;continue}return 0}}function
iK(d,c,g){var
a=g;for(;;){if(a){var
e=a[2],f=a[1];if(b(d,c,f[1])){var
a=e;continue}return[0,f,iK(d,c,e)]}return 0}}function
iL(c,a){return 0===b(p[7][15],c,a)?1:0}function
ro(c,a){var
b=0;function
d(b,a){return iL(a,c)?b:[0,a,b]}return e(g[16],d,b,a)}function
f6(b,a){var
c=a[2],d=a[1];if(0===c)return[0,cX(b,d),0];if(3<=c)return[0,cX(b,d),1];throw[0,F,rp]}function
iM(a){return 0===a?0:3}function
rq(d){var
c=a(g[39],d),e=c[1],f=a(l[22],c[2]);function
h(a){return[1,a]}var
i=b(g[13],h,f);return b(g[40],e,i)}function
iN(c){var
f=c[3],h=c[2],e=a(g[39],c[1]),i=e[1],d=a(l[22],[0,f,e[2]]);if(d){var
j=d[2],k=[1,d[1]],m=function(a){return[1,a]},n=b(g[13],m,j);return[0,b(g[40],i,n),h,k]}throw[0,F,rr]}function
rs(e,d){var
b=iN(d),c=b[3],f=b[2],g=b[1];try{var
i=iy(e,a(p[7][8],g)),j=a(l[17],c),k=a(l[30][7],j),m=a(l[18],c),n=a(l[30][7],m),o=[0,[2,[4,[0,k],i],[5,[0,n]]],iM(f)];return o}catch(b){b=w(b);if(b[1]===fk)return a(h[2],rt);throw b}}function
bS(c){if(typeof
c==="number")return[0,q[2],0];else
switch(c[0]){case
0:var
e=c[1],z=[0,[1,a(l[18],e)]];return[0,a(l[17],e),z];case
1:return[0,q[2],[1,c[1]]];case
2:return a(h[2],ru);case
3:var
f=bS(c[1]);return[0,f[1],[3,f[2]]];case
4:var
g=c[1],A=g[2],i=bS(g[1]),j=i[2],k=i[1],m=bS(A),n=m[2],o=m[1],d=b(q[17],k,o),p=b(q[15],k,d),r=b(q[15],o,d),B=b(q[10],p,r),s=b(q[10],d,B);return 0===b(q[23],s,q[2])?[0,q[2],[4,[0,j,n]]]:[0,s,[4,[0,[6,[0,[0,[1,r]],j]],[6,[0,[0,[1,p]],n]]]]];case
5:return a(h[2],rv);case
6:var
t=c[1],C=t[2],u=bS(t[1]),D=u[2],E=u[1],v=bS(C),F=[6,[0,D,v[2]]];return[0,b(q[10],E,v[1]),F];case
7:return a(h[2],rw);default:var
w=c[1],x=w[2],y=bS(w[1]),G=[8,[0,y[2],x]];return[0,b(q[19],y[1],x),G]}}function
f7(b){var
a=bS(b);return[0,a[1],a[2]]}function
rx(i,g,f){var
d=0,c=0,b=f;for(;;){if(b){var
e=b[2];if(a(i,b[1])){if(d===g)return c;var
d=d+1|0,c=c+1|0,b=e;continue}var
c=c+1|0,b=e;continue}return a(h[2],ry)}}function
bT(c){switch(c[0]){case
0:return[0,q[2],[0,c[1]]];case
1:return[0,q[2],[1,c[1]]];case
2:return[0,q[2],[2,c[1]]];case
3:var
e=c[1],v=[3,[1,a(l[18],e)]];return[0,a(l[17],e),v];case
4:var
f=c[1],w=[4,[1,a(l[18],f)]];return[0,a(l[17],f),w];case
5:var
g=c[1],x=[5,[1,a(l[18],g)]];return[0,a(l[17],g),x];case
6:var
h=f7(c[1]),i=h[1],y=[6,h[2]];return[0,b(q[10],i,i),y];case
7:return[0,q[2],[7,c[1]]];case
8:var
z=c[2],j=f7(c[1]),A=j[2],B=j[1],k=bT(z),C=[8,A,k[2]];return[0,b(q[10],B,k[1]),C];case
9:var
D=c[2],m=bT(c[1]),n=m[1],E=m[2],o=bT(D),p=o[1],F=o[2],d=b(q[17],n,p),r=b(q[15],n,d),s=b(q[15],p,d),G=b(q[10],r,s);return[0,b(q[10],d,G),[9,[10,[4,[1,s]],E],[10,[4,[1,r]],F]]];default:var
H=c[2],t=bT(c[1]),I=t[2],J=t[1],u=bT(H),K=[10,I,u[2]];return[0,b(q[10],J,u[1]),K]}}function
aU(b){if(typeof
b==="number")return[0,a(l[30][8],rz)];else
switch(b[0]){case
0:return[0,a(l[30][8],b[1])];case
1:var
c=b[1],j=kd(e(iO[4],c,1,kc(c)-1|0));return[1,a(l[30][4],j)];case
3:return[5,aU(b[1])];case
4:var
d=b[1],k=d[1],m=aU(d[2]);return[2,aU(k),m];case
5:var
f=b[1],n=f[1],o=aU(f[2]);return[3,aU(n),o];case
6:var
g=b[1],p=g[1],q=aU(g[2]);return[4,aU(p),q];case
8:var
i=b[1],r=i[1],s=a(l[30][3],i[2]);return[6,aU(r),s];default:return a(h[2],rA)}}function
f8(b){var
c=aU(b),d=t[77],e=t[81],f=t[82],g=t[80],h=t[79],i=a(l[30][8],rB),j=a(l[30][8],rC);return bX(t[39],j,i,h,g,f,e,d,c)}function
eL(b){if(b){var
c=b[2],d=b[1];if(c){var
e=eL(c);return[3,[0,a(l[30][1],d)],e]}return[0,a(l[30][1],d)]}return 0}function
rD(c){function
d(c){switch(c[0]){case
0:return[0,a(l[30][1],c[1])];case
1:return[0,a(l[30][1],c[1])];case
2:return[0,a(l[30][1],c[1])];case
6:return[1,f8(c[1])];case
7:return eL(c[1]);case
8:var
g=c[1],h=d(c[2]);return[2,f8(g),h];case
9:var
i=c[1],j=d(c[2]);return[4,d(i),j];case
10:var
k=c[1],m=d(c[2]);return[3,d(k),m];default:var
e=c[1];return 0===b(f[37],e,rE)?0:[5,a(l[30][8],e)]}}return eJ(bx,d(c))}function
aV(b){if(typeof
b==="number")return rF;else
switch(b[0]){case
0:var
k=a(f[49],b[1]);return[0,a(l[30][7],k)];case
1:var
c=b[1],m=kd(e(iO[4],c,1,kc(c)-1|0));return[1,a(l[30][4],m)];case
3:return[5,aV(b[1])];case
4:var
d=b[1],n=d[1],o=aV(d[2]);return[2,aV(n),o];case
5:var
g=b[1],p=g[1],q=aV(g[2]);return[3,aV(p),q];case
6:var
i=b[1],r=i[1],s=aV(i[2]);return[4,aV(r),s];case
8:var
j=b[1],t=j[1],u=a(l[30][3],j[2]);return[6,aV(t),u];default:return a(h[2],rG)}}function
f9(b){var
c=aV(b),d=t[13],e=t[12][6],f=t[12][7],g=t[12][8],h=t[12][5],i=a(l[30][5],1),j=a(l[30][5],0);return bX(t[39],j,i,h,g,f,e,d,c)}function
rH(c){var
e=bT(c)[2];function
d(k){var
c=k;for(;;)switch(c[0]){case
0:return[0,a(l[30][1],c[1])];case
1:return[0,a(l[30][1],c[1])];case
2:return[0,a(l[30][1],c[1])];case
6:return[1,f9(c[1])];case
7:return eL(c[1]);case
8:var
i=c[2],e=c[1];if(typeof
e==="number")var
g=0;else
if(0===e[0])var
j=b(f[26],e[1],rJ),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=d(i);return[2,f9(e),n];case
9:var
o=c[1],p=d(c[2]);return[4,d(o),p];case
10:var
q=c[1],r=d(c[2]);return[3,d(q),r];default:var
h=c[1];if(0===b(f[37],h,rI))return 0;var
m=a(f[49],h);return[5,a(l[30][7],m)]}}return eJ(cW,d(e))}var
iP=[0,function(e,d){var
c=0,b=d;for(;;){if(b){var
f=b[2];if(V(e,b[1]))return c;var
c=c+1|0,b=f;continue}return a(h[2],rK)}}];function
f_(b){var
c=b[1],d=a(l[18],b[2]),f=[0,a(l[30][7],d)];function
h(f,b){var
g=b[2],h=a(p[22][1][8],b[1]);function
c(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}var
d=e(p[5][12],c,h,rL),i=a(l[18],g);return[2,[4,[0,a(l[30][7],i)],d],f]}return e(g[16],h,f,c)}function
aW(d,c){if(typeof
c==="number")return 0;else
switch(c[0]){case
2:return[5,a(l[30][7],c[1])];case
3:var
f=f_(c[1]);return[1,a(t[92],f)];case
4:var
g=c[2],i=f_(c[1]),j=a(t[92],i);return[2,j,aW(d,g)];case
6:var
k=c[1],m=aW(d,c[2]);return[3,aW(d,k),m];case
7:var
n=c[1],o=aW(d,c[2]);return[4,aW(d,n),o];case
0:case
1:var
e=b(iP[1],c[1],d);return[0,a(l[30][1],e)];default:return a(h[2],rM)}}function
cZ(c,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
e=a[3],d=a[2],f=a[1];if(typeof
d!=="number"&&8===d[0]){var
i=d[1],j=cZ([0,f,c],e);return[1,aW(c,i),j]}var
h=cZ([0,f,c],e);return[0,aW(c,d),h]}var
k=a[5],l=a[4],m=a[2],n=[0,a[1],c],o=function(a){return cZ(n,a)},p=b(g[13],o,k),q=aW(c,l);return[2,aW(c,m),q,p]}}function
iQ(e,c){var
f=1+a(p[15],c)|0,d=b(p[18],f,c)[2];if(C[4])H(m[1],h[24],rN,p[13],d);return cZ(e,d)}function
eM(c){var
d=a(C[47][2],c);if(0===d[0])return 0;var
f=d[1];if(C[4])e(m[2],rO,C[13],f);var
i=b(C[48][7],c,f),h=a(g[3],i)[1];if(C[4])e(m[2],rP,p[7][4],h);var
j=a(p[7][8],h);return[0,a(l[22],j)]}function
rQ(d,c){var
e=a(f[40],c);return b(h[49],d,e)}function
rR(d,c){var
e=a(q[33],c);return b(h[49],d,e)}function
eN(g,f){var
e=0,d=g,c=f;for(;;){if(c){if(d){var
i=c[2],j=d[2],k=b(p[20],c[1],d[1]),e=b(p[19],k,e),d=j,c=i;continue}return a(h[2],rS)}return e}}function
iR(d){var
b=a(g[39],d),e=b[2],c=eM(b[1]);return c?[0,eN(e,c[1])]:0}var
iS=C[4]?function(b){a(m[2],rT);a(h[46],h[24]);var
c=iR(b);a(m[2],rU);a(h[46],h[24]);return c}:iR;function
f$(m){var
d=m[2],h=m[1],i=h[3],j=h[2],k=h[1];if(k){var
o=function(a){return a[2]},p=b(g[13],o,k),n=a(l[21],p),c=[1,n];if(b(f[32],c,rV))return[2,h,d];var
q=b(f[12],i,c);if(0===a(f[25],q)){if(1<=a(f[25],c)){var
r=b(f[9],i,c),s=function(a){var
d=a[1];return[0,d,b(f[9],a[2],c)]};return[2,[0,b(g[13],s,k),j,r],[5,n,d]]}throw[0,F,rW]}if(0===j)return[0,[8,d]];var
t=b(f[9],i,c),u=a(f[24],t),v=function(a){var
d=a[1];return[0,d,b(f[9],a[2],c)]};return[1,[0,b(g[13],v,k),j,u],[8,d]]}return e(C[26],j,rX,i)?0:[0,d]}function
iT(k,j,i){var
e=i[1],g=j[1],m=e[2],n=e[1],o=g[2],q=g[1],t=i[2],u=j[2],v=e[3],w=g[3];function
h(d,c){var
e=a(l[18],c),g=b(p[20],e,t),h=a(l[18],d),i=[7,b(p[20],h,u),g],j=b(f[6],v,c),k=b(f[6],w,d),r=b(f[1],k,j),s=b(C[48][3],o,m),x=b(p[7][13],c,n),y=b(p[7][13],d,q);return[0,[0,b(p[7][14],y,x),s,r],i]}var
r=b(p[7][17],k,q),s=b(p[7][17],k,n);if(r)if(s){var
c=s[1],d=r[1],x=a(f[25],c);if(-1===e9(a(f[25],d),x)){var
y=a(f[15],c);return[0,h(y,a(f[15],d))]}if(0===o){var
z=[0,a(f[25],d)],A=b(f[6],c,z),B=a(f[3],A);return[0,h(B,a(f[15],d))]}if(0===m){var
D=a(f[15],c),E=[0,a(f[25],c)],F=b(f[6],d,E);return[0,h(D,a(f[3],F))]}return 0}return 0}var
c0=[a5,rY,a4(0)];function
iU(a){var
b=0;function
c(b,c){var
a=f$([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,c0,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return e(g[16],c,b,a)}function
c1(g,c){if(0===a(q[22],c))return[0,q[2],q[1]];var
d=b(q[14],g,c),h=d[1],e=c1(c,d[2]),f=e[2],i=e[1],j=b(q[10],h,f);return[0,f,b(q[8],i,j)]}function
rZ(j,i){var
c=a(q[35],j),d=a(q[35],i),e=c1(c,d),f=e[2],g=e[1],k=b(q[10],f,d),l=b(q[10],g,c),n=b(q[5],l,k),o=a(q[33],n),p=a(q[33],d),r=a(q[33],f),s=a(q[33],c),t=a(q[33],g);return d9(m[1],h[24],r0,t,s,r,p,o)}var
r2=[a5,r1,a4(0)];function
r3(c){function
b(a){return 0===a[1][2]?1:0}return a(g[32],b)}function
iV(r,p){var
f=p[1],g=r[1];if(0===g[2])if(0===f[2]){var
d=g[1],c=f[1];for(;;){if(d)if(c){var
h=c[2],i=c[1],j=i[2],k=i[1],m=d[2],n=d[1],o=n[2],e=n[1];if(V(e,k)){var
s=q[2],t=a(l[18],j),u=a(l[18],o),v=b(q[17],u,t);if(0===b(q[23],v,s))return[0,[0,e,o,j]];var
d=m,c=h;continue}if(gK(e,k)){var
d=m;continue}var
c=h;continue}return 0}}return 0}function
iW(m,k){var
d=0,c=k;for(;;){if(c){var
f=c[2],e=c[1],n=a(m,e),h=b(l[15],n,f),i=h[1];if(i){var
j=i[1],o=j[2],p=j[1];return[0,[0,[0,p,e,o]],b(g[8],d,h[2])]}var
d=[0,e,d],c=f;continue}return[0,0,d]}}function
iX(a){return iW(iV,a)}function
eO(f,b){var
c=0;function
d(c,d){var
e=a(f,d);if(e){var
b=f$(e[1]);if(typeof
b==="number")return c;else
switch(b[0]){case
0:throw[0,c0,b[1]];case
1:return[0,[0,b[1],b[2]],c];default:return[0,[0,b[1],b[2]],c]}}return[0,d,c]}return e(g[16],d,c,b)}function
eP(c,b,a){return eO(function(a){return iT(c,b,a)},a)}function
iY(r){var
i=iX(r),j=i[1],s=i[2];if(j){var
c=j[1],k=c[3],m=k[1],n=c[2],o=n[2],d=n[1],e=c[1],t=k[2],u=e[2],v=e[1],w=a(l[18],e[3]),q=c1(a(l[18],u),w),g=[1,q[1]],h=[1,q[2]],x=b(f[6],h,m[3]),y=b(f[6],g,d[3]),z=b(f[1],y,x),A=b(p[7][13],h,m[1]),B=b(p[7][13],g,d[1]),C=[0,b(p[7][14],B,A),0,z],D=a(l[18],h),E=b(p[20],D,t),F=a(l[18],g),G=b(p[20],F,o);return[0,eP(v,[0,C,b(p[19],G,E)],[0,[0,d,o],s])]}return 0}function
iZ(e){function
h(c){var
a=c[1];if(0===a[2])try{var
d=a[1],e=function(d){var
a=d[2],c=b(f[26],a,r4);return c?c:b(f[26],a,r5)},h=[0,b(g[29],e,d)[1]];return h}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
a=b(l[15],h,e),c=a[1],i=a[2];if(c){var
d=c[1];return[0,eP(d[1],d[2],i)]}return 0}function
i0(h){function
i(e){var
c=e[1];if(0===c[2])try{var
h=c[1],i=function(c){var
d=c[2],g=c[1],h=b(f[26],d,r6),e=h||b(f[26],d,r7);if(e){var
i=a(p[22][1][8],g);return a(p[5][4],i)}return e},d=b(g[29],i,h)[1],j=a(p[22][1][8],d),k=c[1],l=function(g){var
c=g[1],e=c===d?1:0;if(e)var
f=e;else
var
h=a(p[22][1][8],c),f=0===b(p[5][9],h,j)[2]?1:0;return f},m=b(g[23],l,k)?[0,d]:0;return m}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
c=b(l[15],i,h),d=c[1],j=c[2];if(d){var
e=d[1];return[0,eO(b(p[22][9],e[1],e[2]),j)]}return 0}function
i1(r){function
s(i){var
c=i;for(;;){if(c){var
d=c[2],e=c[1],j=e[1],f=a(l[18],e[2]);try{var
k=function(g){return function(c){var
d=a(l[18],c[2]),e=q[2],f=b(q[17],g,d);return b(q[24],f,e)}}(f),h=b(g[29],k,d),m=h[1],n=[0,[0,[0,j,f],[0,m,a(l[18],h[2])]]];return n}catch(a){a=w(a);if(a===O){var
c=d;continue}throw a}}return 0}}function
t(b){var
a=b[1];return 0===a[2]?s(a[1]):0}var
c=b(l[15],t,r),d=c[1],u=c[2];if(d){var
e=d[1],h=e[2],i=h[1],j=e[1],k=j[2],m=j[1],v=h[2],x=k[1],y=m[1],n=c1(m[2],k[2]),z=[1,n[2]],A=[1,n[1]],o=function(d,c){var
a=b(p[7][17],d,c);return a?a[1]:r8};return[0,eO(function(g){var
c=g[1],d=c[1],h=g[2],j=c[3],k=c[2],l=o(y,d),m=o(x,d),n=b(f[6],m,z),q=b(f[6],l,A),r=b(f[1],q,n),e=a(f[3],r),s=b(f[6],e,i[3]),t=b(f[1],s,j),u=b(p[7][13],e,i[1]);return[0,[0,[0,b(p[7][14],u,d),k,t],[7,[4,[0,0,e],v],h]]]},u)]}return 0}function
r9(i){function
j(c){var
b=c[1];if(0===b[2])try{var
d=[0,a(g[3],b[1])[1]];return d}catch(a){a=w(a);if(a===O)return 0;throw a}return 0}var
d=b(l[15],j,i),e=d[1],k=d[2];if(e){var
h=e[1],c=h[2],n=h[1];if(C[4]){var
o=a(f[40],c[1][3]);H(m[2],r_,p[7][4],c[1][1],o)}return[0,eP(n,c,k)]}return 0}function
ga(e,d){var
b=d;for(;;){var
c=a(e,b);if(c){var
b=c[1];continue}return b}}function
gb(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1],d);if(c)return[0,c[1]];var
b=f;continue}return 0}}function
gc(a){var
b=[0,iZ,[0,iY,[0,i1,0]]];return ga(function(a){return gb(b,a)},a)}function
i2(a){var
b=[0,i0,0];return ga(function(a){return gb(b,a)},a)}function
i3(d){function
B(a){return 0===a[2]?1:0}var
r=b(g[32],B,d),s=r[2],t=r[1];if(t)var
D=0,E=function(c,a){function
d(c){return b(p[7][1],a[1],c[1])}return b(g[24],d,t)?c:[0,a[1],c]},u=e(g[16],E,D,s);else
var
F=function(a){return a[1]},u=b(g[15],F,s);var
G=[0,p[7][3],sa];function
H(c,g){var
h=a(C[6][4],c[2]),n=h?b(f[29],h[1],r$):0;if(n)return c;var
j=b(C[47][1],g,d);if(j){var
k=j[1];if(C[4])e(m[2],sb,p[7][4],g);var
i=c[2],l=c[1];return b(C[6][5],k,i)?[0,g,k]:[0,l,i]}return c}var
v=e(g[16],H,G,u),w=v[2],x=w[1],I=v[1];if(x){var
y=w[2];if(y)var
c=[0,[0,x[1],I,y[1]]],o=1;else
var
o=0}else
var
o=0;if(!o)var
c=0;if(c){var
i=c[1],j=i[3],k=i[2],n=i[1],J=a(l[17],n),K=q[2],L=a(l[18],n),M=b(q[8],L,K),N=a(l[17],j),O=a(l[18],j),P=[1,b(q[5],q[2],O)],z=eM([0,[0,b(p[7][13],[1,N],k),1,P],d]),Q=a(f[3],[1,M]),R=a(f[3],[1,J]),A=eM([0,[0,b(p[7][13],R,k),1,Q],d]);if(z)if(A){var
S=A[1],T=a(g[4],z[1]);return[0,[0,a(g[4],S),[0,n,k,j],T]]}return a(h[2],sc)}return 0}function
eQ(c){function
d(c){var
d=c[1][1];function
e(b){return 0!==a(f[25],b[2])?1:0}return b(g[23],e,d)}return b(g[23],d,c)}function
gd(o,n,c){function
q(j,b){if(C[4]){a(m[2],sd);a(h[46],h[24])}if(eQ(b)){var
k=a(g[39],b),l=k[2],n=i3(k[1]);if(n){var
c=n[1],d=c[2],o=d[3],e=d[2],q=d[1],s=c[3],t=c[1];if(C[4]){var
u=a(f[40],o),v=a(f[40],q);aO(m[2],se,p[7][4],e,v,u)}var
w=a(f[22],o),r=i(j,e,a(f[24],q),w,b);if(r){var
x=r[1],y=eN(l,s);return[0,[1,j,eN(l,t),e,y,x]]}return 0}return 0}throw[0,F,sf]}function
i(c,g,a,e,d){if(b(f[28],a,e))return sg;var
h=j(c+1|0,[0,[0,[0,g,0,a],[1,c]],d]);if(h){var
l=h[1],k=i(c,g,b(f[1],a,sh),e,d);return k?[0,[0,l,k[1]]]:0}return 0}function
j(d,c){if(eQ(c)){if(C[4]){var
h=function(c,a){return b(p[9],c,a[1])},i=a(l[2],h);e(m[2],si,i,c)}try{var
f=a(n,c);if(C[4]){var
j=function(c,a){return b(p[9],c,a[1])},k=a(l[2],j);e(m[2],sj,k,f)}var
g=iS(f),r=g?[0,[0,d,g[1],0]]:o?q(d,f):0;return r}catch(a){a=w(a);if(a[1]===c0)return[0,[0,d,a[2],0]];throw a}}throw[0,F,sk]}var
k=a(g[1],c);try{var
t=j(k,iU(c)),d=t}catch(a){a=w(a);if(a[1]!==c0)throw a;var
d=[0,[0,k,a[2],0]]}if(d){var
r=d[1],s=function(b,a){return a};return[0,iQ(b(l[23],s,c),r)]}return 0}function
ge(b){var
d=b[2],c=a(p[22][5],b[1]),e=c[1];return[0,e,d,a(f[3],c[2])]}function
sl(e,d,c){a(p[22][1][6],0);var
f=cY(c,d);C[10][1]=f;function
h(a){return f6(cW,a)}var
i=b(g[13],h,c),j=b(g[13],ge,i);function
k(b,a){return[0,b,[0,a]]}return gd(e,gc,b(l[23],k,j))}var
aJ=[0,qF,qG,qH,cW,bx,cW,cX,ix,iy,iz,iA,eJ,iB,iC,iD,q1,eK,iE,q6,f4,q_,f5,aT,iF,iG,iH,iI,cY,rf,rg,rh,rj,rk,rl,rm,rn,iK,iL,ro,f6,iM,rq,iN,rs,f7,rx,bT,aU,f8,eL,rD,aV,f9,rH,iP,f_,aW,cZ,iQ,eM,rQ,rR,eN,iS,f$,iT,c0,iU,c1,rZ,r2,r3,iV,iW,iX,eO,eP,iY,iZ,i0,i1,r9,ga,gb,gc,i2,i3,eQ,gd,ge,sl,function(n,m,f){a(p[22][1][6],0);var
o=cY(f,m);C[10][1]=o;function
q(a){return f6(cW,a)}var
r=b(g[13],q,f);function
s(b,a){return[0,b,[0,a]]}var
c=b(l[23],s,r);function
t(b){return a(p[6][13],b[1][1])}var
i=b(g[23],t,c),u=aT[1];function
v(c,b){var
d=b[1][1];function
f(c,f,b){var
d=a(p[5][13],c);return d?e(aT[4],d[1],c,b):b}return e(p[6][9],f,d,c)}var
w=e(g[16],v,u,c);function
x(d,c,b){var
f=a(p[6][4],sm),g=e(p[6][3],d,sn,f),h=a(p[22][5],g),i=a(p[6][4],so);return[0,[0,[0,e(p[6][3],c,sp,i),1],[3,h]],b]}var
d=e(aT[12],x,w,c);if(i)var
j=d;else
var
A=function(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[6,c[2],a[2]],i=b(p[10],e[2],d[2]);return[0,[0,b(p[6][6],g,f),i],h]},B=b(l[8],A,d),j=b(h[22],d,B);function
y(a){var
b=a[2];return[0,ge(a[1]),b]}var
k=b(g[13],y,j);if(eQ(k)){var
z=i?gc:i2;return gd(n,z,k)}throw[0,F,sq]}];a3(746,aJ,"Micromega_plugin.Certificate");var
c2=[0,function(p){var
c=a(br[18],p),l=[a5,sr,a4(0)],f=[a5,ss,a4(0)];function
q(d,b){var
f=a(c[1],d),g=e(G[21],b,st,fa);return[0,a(G[29],g),1,f]}function
r(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
s(b){try{var
c=[0,a(ej[3],b)];return c}catch(b){b=w(b);if(b===su)return 0;if(a(bv[21],b))throw l;throw b}}function
t(c,a){var
d=e(G[32],a,0,1);try{e(G[32],a,0,0);var
f=0===c?4:1;e(G[80],a,f,1);var
g=1,b=g}catch(a){a=w(a);if(a[1]!==G[1])throw a;var
b=0}e(G[32],a,d,0);return b}function
u(a){var
c=e(G[32],a,0,1);try{e(G[32],a,0,0);var
b=e(G[80],a,0,1);return b}catch(b){b=w(b);if(b[1]===G[1]){e(G[32],a,c,0);return 0}throw b}}function
g(d,c,b){return t(d,c)?r(b,function(a){return u(c)}):a(b,0)}function
m(i){var
f=e(G[21],i,sv,fa),j=a(G[28],f),d=a(c[1],gR);function
n(f){for(;;){var
a=s(j);if(a){var
b=a[1];e(c[5],d,b[1],b[2]);continue}return 0}}try{g(0,f,n);a(h[77],j);var
o=e(G[21],i,sy,fa),p=[0,a(G[29],o),1,d];return p}catch(f){f=w(f);if(f===l){a(h[77],j);var
m=e(G[21],i,sw,fa),k=a(G[29],m);g(1,m,function(g){function
f(b,a){return e(ej[1],k,[0,b,a],sx)}b(c[11],f,d);return a(h[46],k)});return[0,k,1,d]}throw f}}function
v(b){var
d=b[1],e=b[3];return 0===b[2]?0:(a(h[59],d),a(c[2],e),b[2]=0,0)}function
n(b,j,i){var
d=b[1],k=b[3];if(0===b[2])throw f;var
l=a(G[31],d);e(c[5],k,j,i);return g(1,l,function(b){e(ej[1],d,[0,j,i],sz);return a(h[46],d)})}function
o(a,d){var
e=a[3];if(0===a[2])throw f;return b(c[7],e,d)}return[0,q,m,o,n,v,function(c,e){var
b=[d,function(b){try{var
a=[0,m(c)];return a}catch(a){return 0}}];return function(c){var
f=j(b),g=k===f?b[1]:d===f?a(i[2],b):b;if(g){var
h=g[1];try{var
m=o(h,c);return m}catch(b){b=w(b);if(b===O){var
l=a(e,c);n(h,c,l);return l}throw b}}return a(e,c)}}]}];a3(749,c2,"Micromega_plugin.Persistent_cache");var
sA=0;function
sB(d,c,b){var
f=a(G[94],0)[1],g=a(c,b),i=a(G[94],0)[1]-f;e(m[2],sC,d,i);a(h[46],h[24]);return g}var
eR=h[7],gf=[0,eR],eS=[0,1],gg=[0,eR];function
gh(a){return[0,eS[1],gg[1]]}function
eT(a){return gf[1]}function
i4(b,a){function
c(b){var
c=b?b[1]:eR;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,1,0,e(g[17],h[16],b,sD),b,d,c]}function
sE(a){eS[1]=a;return 0}var
sH=[0,1,0,sG,sF,function(a){return eS[1]},sE],sJ=i4(sI,gf);b(gi[3],0,sJ);var
sL=i4(sK,gg);b(gi[3],0,sL);b(gi[4],0,sH);function
by(d,c){if(typeof
c==="number")return 0===c?b(h[49],d,sM):b(h[49],d,sN);else
switch(c[0]){case
0:return b(h[49],d,sO);case
1:return H(m[1],d,sP,l[32][3],c[2]);case
2:return D(m[1],d,sQ,by,c[1],by,c[2]);case
3:return D(m[1],d,sR,by,c[1],by,c[2]);case
4:return H(m[1],d,sS,by,c[1]);default:var
e=c[2],f=c[3],g=c[1],i=e?a(I[1][8],e[1]):sU;return d9(m[1],d,sT,by,g,i,by,f)}}function
aX(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
d=b[3],e=b[2];return[1,a(c,b[1]),e,d];case
2:var
f=b[1],g=aX(c,b[2]);return[2,aX(c,f),g];case
3:var
h=b[1],i=aX(c,b[2]);return[3,aX(c,h),i];case
4:return[4,aX(c,b[1])];default:var
j=b[2],k=b[1],l=aX(c,b[3]);return[5,aX(c,k),j,l]}}function
a$(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1],b[2],b[3]];case
2:var
d=b[1],e=a$(c,b[2]);return[2,a$(c,d),e];case
3:var
f=b[1],g=a$(c,b[2]);return[3,a$(c,f),g];case
4:return[4,a$(c,b[1])];default:var
h=b[2],i=b[1],j=a$(c,b[3]);return[5,a$(c,i),h,j]}}function
gj(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];if(b){var
c=b[1];return[0,c,gj(a[3])]}}return 0}var
eU=0,sV=0;function
eV(I,H,m,l,c){function
j(c,a){return b(h[22],c,a)}function
k(c,e){if(e){var
h=e[2],d=e[1],i=b(l,c[1],d[1]);if(i){if(a(m,i[1]))return[0,[0,c[2],[0,d[2],0]]];var
f=k(c,h);return 0===f[0]?[0,f[1]]:[1,[0,d,f[1]]]}var
g=k(c,h);return 0===g[0]?[0,g[1]]:[1,[0,d,g[1]]]}var
j=b(l,c[1],c[1]);return j?a(m,j[1])?[0,[0,c[2],0]]:[1,[0,c,0]]:[1,[0,c,0]]}function
i(a,d){if(a){var
m=a[1],f=i(a[2],d),l=f[2],n=f[1],j=function(l,f){var
g=f[2],i=f[1],a=m,d=l;for(;;){if(a){var
j=a[2],e=k(a[1],d);if(0!==e[0]){var
a=j,d=e[1];continue}var
c=[0,e[1]]}else
var
c=[1,d];return 0===c[0]?[0,i,b(h[22],g,c[1])]:[0,[0,c[1],i],g]}},c=e(g[17],j,d,sW),o=c[1],p=b(h[22],l,c[2]);return[0,b(h[22],n,o),p]}return[0,eU,0]}function
f(O,N){var
c=O,d=N;for(;;)if(typeof
d==="number")return 0===d?c?[0,eU,0]:[0,c3,0]:c?[0,c3,0]:[0,eU,0];else
switch(d[0]){case
0:return c?[0,c3,0]:[0,c3,0];case
1:var
e=d[2],k=d[1],P=0;if(c)var
L=a(H,k),M=function(a){function
c(a){return[0,a,e]}return b(g[13],c,a)},l=b(g[13],M,L);else
var
J=a(I,k),K=function(a){function
c(a){return[0,a,e]}return b(g[13],c,a)},l=b(g[13],K,J);return[0,l,P];case
2:var
Q=d[2],m=f(c,d[1]),n=m[2],o=m[1],p=f(c,Q),q=p[2],r=p[1];if(c){var
R=b(h[22],n,q);return[0,j(o,r),R]}var
s=i(o,r),S=s[1],T=b(h[22],q,s[2]);return[0,S,b(h[22],n,T)];case
3:var
U=d[2],t=f(c,d[1]),u=t[2],v=t[1],w=f(c,U),x=w[2],y=w[1];if(c){var
z=i(v,y),V=z[1],W=b(h[22],x,z[2]);return[0,V,b(h[22],u,W)]}var
X=b(h[22],u,x);return[0,j(v,y),X];case
4:var
c=1-c,d=d[1];continue;default:var
Y=d[3],A=f(1-c,d[1]),B=A[2],C=A[1],D=f(c,Y),E=D[2],F=D[1];if(c){var
G=i(C,F),Z=G[1],_=b(h[22],E,G[2]);return[0,Z,b(h[22],B,_)]}var
$=b(h[22],B,E);return[0,j(C,F),$]}}return f(1,c)}var
au=a(eh[1],[0,$]),sX=a(bO[1],[0,$]);function
i5(f,a){function
d(h,g){var
c=h,a=g;for(;;){if(a){var
e=a[2],i=a[1];if(b(au[3],c,f))return[0,i,d(c+1|0,e)];var
c=c+1|0,a=e;continue}return 0}}return d(0,a)}var
sY=b(h[22],E[9],gk),sZ=b(h[22],E[8],sY),s0=b(h[22],[0,i6,0],sZ),i7=b(h[22],E[10],s0),aq=b(E[6],s1,E[10]),v=b(E[6],s2,i7),aK=b(E[6],s3,i8),ar=b(E[6],s4,i9),aY=b(E[6],s5,i_),aZ=b(E[6],s6,gk),ba=[d,function(b){return a(aq,s7)}],bb=[d,function(b){return a(aq,s8)}],c4=[d,function(b){return a(aq,s9)}],c5=[d,function(b){return a(aq,s_)}],bc=[d,function(b){return a(aq,s$)}],av=[d,function(b){return a(aq,ta)}],c6=[d,function(b){return a(v,tb)}],c7=[d,function(b){return a(v,tc)}],te=[d,function(b){return a(v,td)}],c8=[d,function(b){return a(aq,tf)}],c9=[d,function(b){return a(aq,tg)}],ti=[d,function(b){return a(aq,th)}],c_=[d,function(b){return a(aK,tj)}],c$=[d,function(b){return a(aK,tk)}],da=[d,function(b){return a(aq,tl)}],tn=[d,function(b){return a(aq,tm)}],tp=[d,function(b){return a(aq,to)}],tr=[d,function(b){return a(aK,tq)}],bd=[d,function(b){return a(aK,ts)}],be=[d,function(b){return a(aK,tt)}],bf=[d,function(b){return a(aK,tu)}],bg=[d,function(b){return a(aK,tv)}],db=[d,function(b){return a(aK,tw)}],dc=[d,function(b){return a(aK,tx)}],dd=[d,function(b){return a(aK,ty)}],de=[d,function(b){return a(v,tz)}],bh=[d,function(b){return a(v,tA)}],tC=[d,function(b){return a(v,tB)}],aw=[d,function(b){return a(v,tD)}],tF=[d,function(b){return a(v,tE)}],df=[d,function(b){return a(aZ,tG)}],dg=[d,function(b){return a(aZ,tH)}],dh=[d,function(b){return a(aZ,tI)}],di=[d,function(b){return a(aZ,tJ)}],dj=[d,function(b){return a(aZ,tK)}],dk=[d,function(b){return a(aZ,tL)}],dl=[d,function(b){return a(aZ,tM)}],dm=[d,function(b){return a(aZ,tN)}],dn=[d,function(b){return a(aZ,tO)}],ax=[d,function(b){return a(v,tP)}],ay=[d,function(b){return a(v,tQ)}],tS=[d,function(b){return a(v,tR)}],tU=[d,function(b){return a(v,tT)}],tW=[d,function(b){return a(v,tV)}],tY=[d,function(b){return a(v,tX)}],t0=[d,function(b){return a(v,tZ)}],i$=[d,function(b){return a(aY,t1)}],ja=[d,function(b){return a(aY,t2)}],jb=[d,function(b){return a(aY,t3)}],jc=[d,function(b){return a(aY,t4)}],az=[d,function(b){return a(aq,t5)}],cb=[d,function(b){return a(aY,t6)}],cc=[d,function(b){return a(aY,t7)}],cd=[d,function(b){return a(aY,t8)}],ce=[d,function(b){return a(aY,t9)}],cf=[d,function(b){return a(aY,t_)}],ua=[d,function(b){return a(v,t$)}],uc=[d,function(b){return a(v,ub)}],jd=[d,function(b){return a(v,ud)}],je=[d,function(b){return a(v,ue)}],jf=[d,function(b){return a(v,uf)}],cg=[d,function(b){return a(v,ug)}],ch=[d,function(b){return a(v,uh)}],ci=[d,function(b){return a(v,ui)}],cj=[d,function(b){return a(v,uj)}],ck=[d,function(b){return a(v,uk)}],jg=[d,function(b){return a(ar,ul)}],jh=[d,function(b){return a(ar,um)}],ji=[d,function(b){return a(ar,un)}],jj=[d,function(b){return a(ar,uo)}],ah=[d,function(b){return a(ar,up)}],aL=[d,function(b){return a(ar,uq)}],a0=[d,function(b){return a(ar,ur)}],ai=[d,function(b){return a(ar,us)}],uu=[d,function(b){return a(ar,ut)}],bi=[d,function(b){return a(ar,uv)}],cl=[d,function(b){return a(ar,uw)}],bj=[d,function(b){return a(ar,ux)}],bk=[d,function(b){return a(ar,uy)}],dp=[d,function(b){return a(v,uz)}],dq=[d,function(b){return a(v,uA)}],dr=[d,function(b){return a(v,uB)}],ds=[d,function(b){return a(v,uC)}],dt=[d,function(b){return a(v,uD)}],du=[d,function(b){return a(v,uE)}],dv=[d,function(b){return a(v,uF)}],dw=[d,function(b){return a(v,uG)}],dx=[d,function(b){return a(v,uH)}],dy=[d,function(b){return a(v,uI)}],dz=[d,function(b){return a(v,uJ)}],dA=[d,function(b){return a(v,uK)}],dB=[d,function(b){return a(v,uL)}],dC=[d,function(b){return a(v,uM)}],dD=[d,function(b){return a(v,uN)}],dE=[d,function(b){return a(v,uO)}],dF=[d,function(b){return a(v,uP)}],dG=[d,function(b){return a(v,uQ)}],dH=[d,function(b){return a(v,uR)}],dI=[d,function(b){return a(v,uS)}],dJ=[d,function(b){return a(v,uT)}],dK=[d,function(b){return a(v,uU)}],dL=[d,function(b){return a(v,uV)}],uX=[d,function(b){return a(v,uW)}],u1=[d,function(a){return e(E[6],u0,uZ,uY)}],u5=[d,function(a){return e(E[6],u4,u3,u2)}],dM=[d,function(a){return e(E[6],u8,u7,u6)}],dN=[d,function(a){return e(E[6],u$,u_,u9)}],dO=[d,function(a){return e(E[6],vc,vb,va)}],dP=[d,function(a){return e(E[6],vf,ve,vd)}],dQ=[d,function(a){return e(E[6],vi,vh,vg)}],dR=[d,function(a){return e(E[6],vl,vk,vj)}],dS=[d,function(a){return e(E[6],vo,vn,vm)}],dT=[d,function(a){return e(E[6],vr,vq,vp)}],vv=[d,function(a){return e(E[6],vu,vt,vs)}],vz=[d,function(a){return e(E[6],vy,vx,vw)}],vD=[d,function(a){return e(E[6],vC,vB,vA)}],vH=[d,function(a){return e(E[6],vG,vF,vE)}],dU=[d,function(a){return e(E[6],vK,vJ,vI)}],vO=[d,function(a){return e(E[6],vN,vM,vL)}];function
vP(b){if(typeof
b==="number")return vQ;else
switch(b[0]){case
0:return b[1];case
1:return a(h[20],b[1]);case
2:return vR;case
3:return b[1];default:return vS}}var
P=[a5,vT,a4(0)];function
cm(d){var
b=a(o[aP],d);switch(b[0]){case
9:var
e=b[2],c=a(o[aP],b[1]);if(12===c[0])return[0,c[1][1][2],e];throw P;case
12:return[0,b[1][1][2],[0]];default:throw P}}function
gl(c){var
a=cm(c),b=a[1],d=a[2];if(1===b)return 0;if(2===b)return[0,gl(z(d,0)[1])];throw P}function
jk(c,b){var
d=a(l[29][1],b);return e(m[1],c,vU,d)}function
eW(b){if(b){var
c=j(c9),f=[0,eW(b[1])],g=k===c?c9[1]:d===c?a(i[2],c9):c9;return a(o[s],[0,g,f])}var
e=j(c8);return k===e?c8[1]:d===e?a(i[2],c8):c8}function
cn(d){var
a=cm(d),b=a[2],c=a[1]-1|0;if(2<c>>>0)throw P;switch(c){case
0:return[0,cn(z(b,0)[1])];case
1:return[1,cn(z(b,0)[1])];default:return 0}}function
aA(b){if(typeof
b==="number"){var
c=j(bd);return k===c?bd[1]:d===c?a(i[2],bd):bd}else{if(0===b[0]){var
e=j(bf),g=[0,aA(b[1])],h=k===e?bf[1]:d===e?a(i[2],bf):bf;return a(o[s],[0,h,g])}var
f=j(be),l=[0,aA(b[1])],m=k===f?be[1]:d===f?a(i[2],be):be;return a(o[s],[0,m,l])}}function
dV(c,b){var
d=a(l[29][2],b);return e(m[1],c,vV,d)}function
jl(b){if(b){var
c=j(c$),f=[0,aA(b[1])],g=k===c?c$[1]:d===c?a(i[2],c$):c$;return a(o[s],[0,g,f])}var
e=j(c_);return k===e?c_[1]:d===e?a(i[2],c_):c_}function
gm(b){if(typeof
b==="number"){var
c=j(bd);return k===c?bd[1]:d===c?a(i[2],bd):bd}else{if(0===b[0]){var
e=j(bf),g=[0,gm(b[1])],h=k===e?bf[1]:d===e?a(i[2],bf):bf;return a(o[s],[0,h,g])}var
f=j(be),l=[0,gm(b[1])],m=k===f?be[1]:d===f?a(i[2],be):be;return a(o[s],[0,m,l])}}function
vW(c,b){var
d=a(l[29][4],b);return e(m[1],c,vX,d)}function
jm(d,c){var
e=a(l[29][3],c),f=a(h[20],e);return b(h[49],d,f)}function
vY(h,g,f,e,b){var
l=b[1],m=a(e,b[2]),c=j(da),n=[0,h,g,a(f,l),m],p=k===c?da[1]:d===c?a(i[2],da):da;return a(o[s],[0,p,n])}function
bU(d){var
a=cm(d),b=a[2],c=a[1]-1|0;if(2<c>>>0)throw P;switch(c){case
0:return 0;case
1:return[0,cn(z(b,0)[1])];default:return[1,cn(z(b,0)[1])]}}function
bz(b){if(typeof
b==="number"){var
c=j(db);return k===c?db[1]:d===c?a(i[2],db):db}else{if(0===b[0]){var
e=j(dc),g=[0,aA(b[1])],h=k===e?dc[1]:d===e?a(i[2],dc):dc;return a(o[s],[0,h,g])}var
f=j(dd),l=[0,aA(b[1])],m=k===f?dd[1]:d===f?a(i[2],dd):dd;return a(o[s],[0,m,l])}}function
jn(c,b){var
d=a(l[29][7],b),f=a(q[33],d);return e(m[1],c,vZ,f)}function
v0(b){var
e=a(l[17],b),f=aA(a(l[30][6],e)),g=a(l[18],b),c=j(aw),h=[0,bz(a(l[30][7],g)),f],m=k===c?aw[1]:d===c?a(i[2],aw):aw;return a(o[s],[0,m,h])}function
eX(b){var
e=aA(b[2]),c=j(aw),f=[0,bz(b[1]),e],g=k===c?aw[1]:d===c?a(i[2],aw):aw;return a(o[s],[0,g,f])}function
dW(g){var
c=a(o[aP],g);if(9===c[0]){var
e=c[2],f=j(aw),h=c[1],l=k===f?aw[1]:d===f?a(i[2],aw):aw;if(b(aM[27],h,l)){var
m=cn(z(e,1)[2]);return[0,bU(z(e,0)[1]),m]}throw P}throw P}function
bl(c,a){if(typeof
a==="number")return 0===a?b(h[49],c,v1):b(h[49],c,v2);else
switch(a[0]){case
0:return b(h[49],c,v3);case
1:return jn(c,a[1]);case
2:return D(m[1],c,v4,bl,a[1],bl,a[2]);case
3:return D(m[1],c,v5,bl,a[1],bl,a[2]);case
4:return D(m[1],c,v6,bl,a[1],bl,a[2]);case
5:return H(m[1],c,v7,bl,a[1]);default:return H(m[1],c,v8,bl,a[1])}}function
bm(b){if(typeof
b==="number"){if(0===b){var
c=j(df);return k===c?df[1]:d===c?a(i[2],df):df}var
e=j(dg);return k===e?dg[1]:d===e?a(i[2],dg):dg}else
switch(b[0]){case
0:var
f=j(dh),q=[0,eX(b[1])],r=k===f?dh[1]:d===f?a(i[2],dh):dh;return a(o[s],[0,r,q]);case
1:var
g=j(di),t=[0,bz(b[1])],u=k===g?di[1]:d===g?a(i[2],di):di;return a(o[s],[0,u,t]);case
2:var
v=b[1],w=bm(b[2]),h=j(dj),x=[0,bm(v),w],y=k===h?dj[1]:d===h?a(i[2],dj):dj;return a(o[s],[0,y,x]);case
3:var
z=b[1],A=bm(b[2]),l=j(dk),B=[0,bm(z),A],C=k===l?dk[1]:d===l?a(i[2],dk):dk;return a(o[s],[0,C,B]);case
4:var
D=b[1],E=bm(b[2]),m=j(dl),F=[0,bm(D),E],G=k===m?dl[1]:d===m?a(i[2],dl):dl;return a(o[s],[0,G,F]);case
5:var
n=j(dm),H=[0,bm(b[1])],I=k===n?dm[1]:d===n?a(i[2],dm):dm;return a(o[s],[0,I,H]);default:var
p=j(dn),J=[0,bm(b[1])],K=k===p?dn[1]:d===p?a(i[2],dn):dn;return a(o[s],[0,K,J])}}function
bn(d){var
b=cm(d),a=b[2],c=b[1]-1|0;if(7<c>>>0)throw P;switch(c){case
0:return 0;case
1:return 1;case
2:return[0,dW(z(a,0)[1])];case
3:var
e=bn(z(a,1)[2]);return[2,bn(z(a,0)[1]),e];case
4:var
f=bn(z(a,1)[2]);return[3,bn(z(a,0)[1]),f];case
5:var
g=bn(z(a,1)[2]);return[4,bn(z(a,0)[1]),g];case
6:return[5,bn(z(a,0)[1])];default:return[6,bn(z(a,0)[1])]}}function
jo(b,f){var
c=cm(f),d=c[2],e=c[1];if(1===e)return 0;if(2===e){var
g=jo(b,z(d,2)[3]);return[0,a(b,z(d,1)[2]),g]}throw P}function
jp(c,e,b){if(b){var
h=b[1],l=jp(c,e,b[2]),f=j(c6),m=[0,c,a(e,h),l],n=k===f?c6[1]:d===f?a(i[2],c6):c6;return a(o[s],[0,n,m])}var
g=j(c7),p=[0,c],q=k===g?c7[1]:d===g?a(i[2],c7):c7;return a(o[s],[0,q,p])}function
v9(f,e,b,d,a){function
c(d,a){if(a){var
e=a[2],f=a[1];return e?D(m[1],d,v_,b,f,c,e):H(m[1],d,v$,b,f)}return 0}return D(m[1],d,wa,f,c,a,e)}function
gn(e,d,a){function
c(d,a){switch(a[0]){case
0:return b(e,d,a[1]);case
1:return H(m[1],d,wb,dV,a[1]);case
2:return D(m[1],d,wc,c,a[1],c,a[2]);case
3:return D(m[1],d,wd,c,a[1],c,a[2]);case
4:return D(m[1],d,we,c,a[1],c,a[2]);case
5:return H(m[1],d,wf,c,a[1]);default:return D(m[1],d,wg,c,a[1],jm,a[2])}}return c(d,a)}function
go(e,q,b){function
c(b){switch(b[0]){case
0:var
f=j(dq),r=[0,e,a(q,b[1])],t=k===f?dq[1]:d===f?a(i[2],dq):dq;return a(o[s],[0,t,r]);case
1:var
g=j(dp),u=[0,e,aA(b[1])],v=k===g?dp[1]:d===g?a(i[2],dp):dp;return a(o[s],[0,v,u]);case
2:var
w=b[1],x=c(b[2]),h=j(dr),y=[0,e,c(w),x],z=k===h?dr[1]:d===h?a(i[2],dr):dr;return a(o[s],[0,z,y]);case
3:var
A=b[1],B=c(b[2]),l=j(du),C=[0,e,c(A),B],D=k===l?du[1]:d===l?a(i[2],du):du;return a(o[s],[0,D,C]);case
4:var
E=b[1],F=c(b[2]),m=j(dt),G=[0,e,c(E),F],H=k===m?dt[1]:d===m?a(i[2],dt):dt;return a(o[s],[0,H,G]);case
5:var
n=j(ds),I=[0,e,c(b[1])],J=k===n?ds[1]:d===n?a(i[2],ds):ds;return a(o[s],[0,J,I]);default:var
K=b[1],L=jl(b[2]),p=j(dv),M=[0,e,c(K),L],N=k===p?dv[1]:d===p?a(i[2],dv):dv;return a(o[s],[0,N,M])}}return c(b)}function
gp(e,l,b){function
c(b){switch(b[0]){case
0:var
f=j(dx),m=[0,e,a(l,b[1])],n=k===f?dx[1]:d===f?a(i[2],dx):dx;return a(o[s],[0,n,m]);case
1:var
p=b[1],q=c(b[2]),g=j(dy),r=[0,e,aA(p),q],t=k===g?dy[1]:d===g?a(i[2],dy):dy;return a(o[s],[0,t,r]);default:var
u=b[2],v=b[1],w=c(b[3]),x=aA(u),h=j(dw),y=[0,e,c(v),x,w],z=k===h?dw[1]:d===h?a(i[2],dw):dw;return a(o[s],[0,z,y])}}return c(b)}function
eY(d,c,a){function
b(c,a){switch(a[0]){case
0:return H(m[1],c,wh,d,a[1]);case
1:return D(m[1],c,wi,dV,a[1],b,a[2]);default:return bX(m[1],c,wj,b,a[1],dV,a[2],b,a[3])}}return b(c,a)}function
wk(d,c,a){function
e(c,a){function
e(a){var
b=a[2],e=a[1][1],f=l[32][3];function
g(a,b){return eY(d,a,b)}return D(m[1],c,wl,g,e,f,b)}return b(g[11],e,a)}function
f(a){return H(m[1],c,wm,e,a)}return b(g[11],f,a)}function
wn(b,f,h){var
g=j(b),c=k===g?b[1]:d===g?a(i[2],b):b;function
e(b){if(typeof
b==="number"){var
g=j(dL),r=[0,c],t=k===g?dL[1]:d===g?a(i[2],dL):dL;return a(o[s],[0,t,r])}else
switch(b[0]){case
0:var
h=j(dF),u=[0,c,eW(b[1])],v=k===h?dF[1]:d===h?a(i[2],dF):dF;return a(o[s],[0,v,u]);case
1:var
l=j(dG),w=[0,c,gp(c,f,b[1])],x=k===l?dG[1]:d===l?a(i[2],dG):dG;return a(o[s],[0,x,w]);case
2:var
y=b[1],z=e(b[2]),m=j(dI),A=[0,c,gp(c,f,y),z],B=k===m?dI[1]:d===m?a(i[2],dI):dI;return a(o[s],[0,B,A]);case
3:var
C=b[1],D=e(b[2]),n=j(dH),E=[0,c,e(C),D],F=k===n?dH[1]:d===n?a(i[2],dH):dH;return a(o[s],[0,F,E]);case
4:var
G=b[1],H=e(b[2]),p=j(dJ),I=[0,c,e(G),H],J=k===p?dJ[1]:d===p?a(i[2],dJ):dJ;return a(o[s],[0,J,I]);default:var
q=j(dK),K=[0,c,a(f,b[1])],L=k===q?dK[1]:d===q?a(i[2],dK):dK;return a(o[s],[0,L,K])}}return e(h)}function
wo(e,c,a){function
d(c,a){if(typeof
a==="number")return b(m[1],c,wp);else
switch(a[0]){case
0:return H(m[1],c,wq,jk,a[1]);case
1:var
f=a[1],g=function(a,b){return eY(e,a,b)};return H(m[1],c,wr,g,f);case
2:var
h=a[2],i=a[1],j=function(a,b){return eY(e,a,b)};return D(m[1],c,ws,j,i,d,h);case
3:return D(m[1],c,wt,d,a[1],d,a[2]);case
4:return D(m[1],c,wu,d,a[1],d,a[2]);default:return H(m[1],c,wv,e,a[1])}}return d(c,a)}function
jq(l){switch(l){case
0:var
b=j(dz);return k===b?dz[1]:d===b?a(i[2],dz):dz;case
1:var
c=j(dA);return k===c?dA[1]:d===c?a(i[2],dA):dA;case
2:var
e=j(dB);return k===e?dB[1]:d===e?a(i[2],dB):dB;case
3:var
f=j(dD);return k===f?dD[1]:d===f?a(i[2],dD):dD;case
4:var
g=j(dC);return k===g?dC[1]:d===g?a(i[2],dC):dC;default:var
h=j(dE);return k===h?dE[1]:d===h?a(i[2],dE):dE}}function
jr(a,c){switch(c){case
0:return b(m[1],a,ww);case
1:return b(m[1],a,wx);case
2:return b(m[1],a,wy);case
3:return b(m[1],a,wz);case
4:return b(m[1],a,wA);default:return b(m[1],a,wB)}}function
wC(b,c,a){var
d=a[3],e=a[2],f=a[1];function
g(a,c){return gn(b,a,c)}function
h(a,c){return gn(b,a,c)}return bX(m[1],c,wD,h,f,jr,e,g,d)}function
wE(c,e,b){var
g=b[2],h=b[1],l=go(c,e,b[3]),m=jq(g),f=j(dU),n=[0,c,go(c,e,h),m,l],p=k===f?dU[1]:d===f?a(i[2],dU):dU;return a(o[s],[0,p,n])}function
dX(f,c){try{var
e=function(g){var
c=g[1],e=j(c),h=k===e?c[1]:d===e?a(i[2],c):c;return b(aM[27],f,h)},h=b(g[29],e,c)[2];return h}catch(a){a=w(a);if(a===O)throw P;throw a}}var
gq=[0,[0,i$,5],[0,[0,ja,3],[0,[0,jc,4],[0,[0,jb,2],0]]]],gr=[0,[0,jg,5],[0,[0,jh,3],[0,[0,jj,4],[0,[0,ji,2],0]]]],gs=[0,[0,je,4],[0,[0,jd,2],[0,[0,jf,0],0]]];function
wF(c,e,d){var
f=a(ae[2],c),g=a(ae[8],c),h=aO(js[2],0,0,g,f,e);return b(aM[27],h,d)}function
gt(b,d,c){var
e=a(ae[2],b),f=a(ae[8],b);return aO(wG[77],0,f,e,d,c)}function
jt(n,f){var
c=f[2],e=f[1],g=a(o[aP],e);switch(g[0]){case
10:var
p=z(c,1)[2],q=z(c,0)[1];return[0,dX(e,gq),q,p];case
11:if(0===g[1][1][2]){var
l=j(az),r=k===l?az[1]:d===l?a(i[2],az):az;if(b(aM[27],e,r)){var
m=j(bg),s=k===m?bg[1]:d===m?a(i[2],bg):bg;if(gt(n,z(c,0)[1],s)){var
t=z(c,2)[3];return[0,0,z(c,1)[2],t]}}throw P}break}return a(h[2],wH)}function
ju(n,f){var
c=f[2],e=f[1],g=a(o[aP],e);switch(g[0]){case
10:var
p=z(c,1)[2],q=z(c,0)[1];return[0,dX(e,gr),q,p];case
11:if(0===g[1][1][2]){var
l=j(az),r=k===l?az[1]:d===l?a(i[2],az):az;if(b(aM[27],e,r)){var
m=j(bh),s=k===m?bh[1]:d===m?a(i[2],bh):bh;if(gt(n,z(c,0)[1],s)){var
t=z(c,2)[3];return[0,0,z(c,1)[2],t]}}throw P}break}return a(h[2],wI)}function
jv(f,a){var
b=a[2],c=a[1],d=z(b,1)[2],e=z(b,0)[1];return[0,dX(c,gs),e,d]}function
wJ(b){return 12===a(o[aP],b)[0]?1:0}function
jw(f,c){try{var
e=function(g){var
c=g[1],e=j(c),h=k===e?c[1]:d===e?a(i[2],c):c;return b(aM[27],f,h)},h=b(g[29],e,c)[2];return h}catch(a){a=w(a);if(a===O)return wK;throw a}}function
wL(f,d){function
e(a,d,c){if(a){var
f=a[1],h=a[2];if(b(o[bD],f,c))return[0,a,d];var
g=e(h,d+1|0,c);return[0,[0,f,g[1]],g[2]]}return[0,[0,c,0],d]}var
c=e(f,1,d),g=c[1];return[0,g,a(l[30][2],c[2])]}function
wM(e,d){var
a=e,c=1;for(;;){if(a){var
f=a[2];if(b(o[bD],a[1],d))return c;var
a=f,c=c+1|0;continue}throw[0,jx,wN]}}var
wO=0,bV=[0,wL,wM,wO,function(a){return a}];function
eZ(g,r,q,d,c){function
j(d,c){var
a=b(bV[1],d,c);return[0,[1,a[2]],a[1]]}function
e(c,d){function
s(g,f,a){var
h=a[2],c=e(g,a[1]),i=c[1],d=e(c[2],h),j=d[2];return[0,b(f,i,d[1]),j]}try{var
A=[0,[0,a(g,d)],c];return A}catch(g){g=w(g);if(g===P){var
i=a(o[aP],d);if(9===i[0]){var
f=i[2],k=i[1];if(10===a(o[aP],k)[0]){var
h=jw(k,q);if(typeof
h==="number"){if(0===h){var
l=e(c,z(f,0)[1]);return[0,[5,l[1]],l[2]]}try{var
n=e(c,z(f,0)[1]),t=n[2],u=n[1],v=[0,b(r,u,z(f,1)[2]),t];return v}catch(e){e=w(e);if(a(bv[21],e)){var
m=b(bV[1],c,d);return[0,[1,m[2]],m[1]]}throw e}}else{if(0===h[0]){var
x=h[1],y=z(f,1)[2];return s(c,x,[0,z(f,0)[1],y])}var
p=b(bV[1],c,d);return[0,[1,p[2]],p[1]]}}return j(c,d)}return j(c,d)}throw g}}return e(d,c)}var
wP=[0,[0,cd,0],[0,[0,cf,1],0]],wQ=[0,[0,ce,[0,function(b,a){return[4,b,a]}]],wP],wR=[0,[0,cc,[0,function(b,a){return[3,b,a]}]],wQ],jy=[0,[0,cb,[0,function(b,a){return[2,b,a]}]],wR],wS=[0,[0,ci,0],[0,[0,ck,1],0]],wT=[0,[0,cj,[0,function(b,a){return[4,b,a]}]],wS],wU=[0,[0,ch,[0,function(b,a){return[3,b,a]}]],wT],jz=[0,[0,cg,[0,function(b,a){return[2,b,a]}]],wU],wV=[0,[0,a0,0],[0,[0,cl,1],0]],wW=[0,[0,ai,[0,function(b,a){return[4,b,a]}]],wV],wX=[0,[0,aL,[0,function(b,a){return[3,b,a]}]],wW],jA=[0,[0,ah,[0,function(b,a){return[2,b,a]}]],wX],wY=0,wZ=[0,[0,ai,function(b,a){return[4,b,a]}],wY],w0=[0,[0,aL,function(b,a){return[3,b,a]}],wZ],jB=[0,[0,ah,function(b,a){return[2,b,a]}],w0];function
e0(f){var
g=a(o[aP],f);switch(g[0]){case
9:var
c=g[2],e=g[1];try{var
x=dX(e,jB),y=e0(z(c,0)[1]),A=b(x,y,e0(z(c,1)[2]));return A}catch(f){f=w(f);if(f===P){var
h=j(bi),r=k===h?bi[1]:d===h?a(i[2],bi):bi;if(b(aM[27],e,r)){var
l=e0(z(c,0)[1]),s=a(t[gV],l);if(b(t[77],s,w1))throw P;return[5,l]}var
m=j(bk),u=k===m?bk[1]:d===m?a(i[2],bk):bk;if(b(aM[27],e,u))return[0,dW(z(c,0)[1])];var
n=j(bj),v=k===n?bj[1]:d===n?a(i[2],bj):bj;if(b(aM[27],e,v))return[1,bU(z(c,0)[1])];throw P}throw f}case
10:var
p=j(ax),B=k===p?ax[1]:d===p?a(i[2],ax):ax;if(b(aM[27],f,B))return 0;var
q=j(ay),C=k===q?ay[1]:d===q?a(i[2],ay):ay;if(b(aM[27],f,C))return 1;throw P;default:throw P}}function
jC(a){return e0(a)}function
w2(d,c){var
b=bU(c);if(typeof
b!=="number"&&1===b[0])return w3;return[6,d,a(t[12][15],b)]}function
jD(a,b){return eZ(bU,w2,jy,a,b)}function
w4(d,e){var
c=bU(e);if(typeof
c!=="number"&&1===c[0]){if(0===d[0])return[0,b(t[85],d[1],c)];a(h[27],w5);a(h[46],h[24]);throw P}return[6,d,a(t[12][15],c)]}function
jE(a,b){return eZ(dW,w4,jz,a,b)}function
w6(c,b){var
d=gl(b);return[6,c,a(t[7][1],d)]}function
jF(a,b){return eZ(jC,w6,jA,a,b)}function
e1(l,e,k,j,i){var
c=a(o[aP],j);if(9===c[0]){var
d=b(l,i,[0,c[1],c[2]]),m=d[3],n=d[1],f=b(e,k,d[2]),p=f[1],g=b(e,f[2],m);return[0,[0,p,n,g[1]],g[2]]}return a(h[2],w7)}function
w8(a,b,c){return e1(jt,jD,a,b,c)}function
w9(a,b,c){return e1(jv,jE,a,b,c)}function
w_(a,b,c){return e1(ju,jF,a,b,c)}function
bB(a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return 2;case
1:return[0,a[1]];case
2:var
b=a[1],c=bB(a[2]);return[1,bB(b),c];case
3:var
d=a[1],e=bB(a[2]);return[2,bB(d),e];case
4:return[3,bB(a[1])];default:var
f=a[1],g=bB(a[3]);return[4,bB(f),g]}}function
jG(b,a){return[2,b,a]}function
jH(b,a){return[3,b,a]}function
jI(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
jJ(b,a){return[5,b,0,a]}function
dY(e,d,c,a){if(typeof
c!=="number"&&0===c[0])if(typeof
a!=="number"&&0===a[0])return[0,d];return b(e,c,a)}function
w$(n,m,h,g,f){function
J(f,d,c){try{var
b=e(m,f,c,n),g=b[2],h=b[1],i=[0,[1,h,d,c],g,a(l[32][2],d)];return i}catch(b){b=w(b);if(a(bv[21],b))return[0,[0,c],f,d];throw b}}function
c(g,f,e){var
h=a(o[aP],e);switch(h[0]){case
6:var
C=h[3],Q=h[2],R=a(o[gT],1);if(!b(jK[45],R,C)){var
p=c(g,f,Q),S=p[1],q=c(p[2],p[3],C),T=q[3],U=q[2];return[0,dY(jJ,e,S,q[1]),U,T]}break;case
9:var
l=h[2],m=h[1],D=l.length-1;if(!(3<=D))switch(D){case
0:break;case
1:var
E=j(c4),V=l[1],W=k===E?c4[1]:d===E?a(i[2],c4):c4;if(b(o[bD],m,W)){var
r=c(g,f,V);return[0,[4,r[1]],r[2],r[3]]}break;default:var
s=l[1],t=l[2],F=j(ba),X=k===F?ba[1]:d===F?a(i[2],ba):ba;if(b(o[bD],m,X)){var
u=c(g,f,s),Y=u[1],v=c(u[2],u[3],t),Z=v[3],_=v[2];return[0,dY(jG,e,Y,v[1]),_,Z]}var
G=j(bb),$=k===G?bb[1]:d===G?a(i[2],bb):bb;if(b(o[bD],m,$)){var
w=c(g,f,s),aa=w[1],x=c(w[2],w[3],t),ab=x[3],ac=x[2];return[0,dY(jH,e,aa,x[1]),ac,ab]}var
I=j(c5),ad=k===I?c5[1]:d===I?a(i[2],c5):c5;if(b(o[bD],m,ad)){var
y=c(g,f,s),af=y[1],z=c(y[2],y[3],t),ag=z[3],ah=z[2];return[0,dY(jI,e,af,z[1]),ah,ag]}}return J(g,f,e)}var
A=j(bc),N=k===A?bc[1]:d===A?a(i[2],bc):bc;if(b(o[bD],e,N))return[0,0,g,f];var
B=j(av),O=k===B?av[1]:d===B?a(i[2],av):av;if(b(o[bD],e,O))return[0,1,g,f];var
K=a(ae[2],n),L=a(ae[8],n),M=H(js[3],0,L,K,e);if(a(o[106],M))return[0,[0,e],g,f];throw P}return c(h,g,f)}function
xa(c,r,b){function
e(b){if(typeof
b==="number"){if(0===b){var
f=j(dM),t=[0,c],u=k===f?dM[1]:d===f?a(i[2],dM):dM;return a(o[s],[0,u,t])}var
g=j(dN),v=[0,c],w=k===g?dN[1]:d===g?a(i[2],dN):dN;return a(o[s],[0,w,v])}else
switch(b[0]){case
0:var
h=j(dS),x=[0,c,b[1]],y=k===h?dS[1]:d===h?a(i[2],dS):dS;return a(o[s],[0,y,x]);case
1:var
l=j(dR),z=[0,c,a(r,b[1])],A=k===l?dR[1]:d===l?a(i[2],dR):dR;return a(o[s],[0,A,z]);case
2:var
B=b[1],C=e(b[2]),m=j(dO),D=[0,c,e(B),C],E=k===m?dO[1]:d===m?a(i[2],dO):dO;return a(o[s],[0,E,D]);case
3:var
F=b[1],G=e(b[2]),n=j(dP),H=[0,c,e(F),G],I=k===n?dP[1]:d===n?a(i[2],dP):dP;return a(o[s],[0,I,H]);case
4:var
p=j(dQ),J=[0,c,e(b[1])],K=k===p?dQ[1]:d===p?a(i[2],dQ):dQ;return a(o[s],[0,K,J]);default:var
L=b[1],M=e(b[3]),q=j(dT),N=[0,c,e(L),M],O=k===q?dT[1]:d===q?a(i[2],dT):dT;return a(o[s],[0,O,N])}}return e(b)}function
jL(a){function
e(i,h){var
c=i,a=h;for(;;){if(typeof
a==="number")var
d=0;else
switch(a[0]){case
0:return b(bV[1],c,a[1])[1];case
4:var
a=a[1];continue;case
5:var
g=a[3],f=a[1],d=1;break;case
1:var
d=0;break;default:var
g=a[2],f=a[1],d=1}if(d){var
c=e(c,f),a=g;continue}return c}}return e(0,a)}function
jM(c){function
d(e){var
c=e;for(;;)switch(c[0]){case
0:return au[1];case
1:var
f=a(l[29][2],c[1]);return a(au[5],f);case
5:case
6:var
c=c[1];continue;default:var
g=c[1],h=d(c[2]),i=d(g);return b(au[7],i,h)}}function
e(l){var
a=l;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
1:var
f=a[1],i=f[1],j=d(f[3]),k=d(i);return b(au[7],k,j);case
4:var
a=a[1];continue;case
5:var
h=a[3],g=a[1],c=1;break;case
0:var
c=0;break;default:var
h=a[2],g=a[1],c=1}if(c){var
m=e(h),n=e(g);return b(au[7],n,m)}return au[1]}}return e(c)}var
xb=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gq),c=j(cf);function
q(b){var
c=a(l[29][3],b);return bz(a(l[30][5],c))}var
r=k===c?cf[1]:d===c?a(i[2],cf):cf,e=j(ce),s=k===e?ce[1]:d===e?a(i[2],ce):ce,f=j(cd),t=k===f?cd[1]:d===f?a(i[2],cd):cd,h=j(cc),u=k===h?cc[1]:d===h?a(i[2],cc):cc,m=j(cb),v=k===m?cb[1]:d===m?a(i[2],cb):cb,n=j(bg),w=k===n?bg[1]:d===n?a(i[2],bg):bg;return[0,w,bz,v,u,t,s,r,q,p]}],xc=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gs),c=j(ck);function
q(b){var
c=a(l[29][3],b);return bz(a(l[30][5],c))}var
r=k===c?ck[1]:d===c?a(i[2],ck):ck,e=j(cj),s=k===e?cj[1]:d===e?a(i[2],cj):cj,f=j(ci),t=k===f?ci[1]:d===f?a(i[2],ci):ci,h=j(ch),u=k===h?ch[1]:d===h?a(i[2],ch):ch,m=j(cg),v=k===m?cg[1]:d===m?a(i[2],cg):cg,n=j(de),w=k===n?de[1]:d===n?a(i[2],de):de;return[0,w,eX,v,u,t,s,r,q,p]}];function
jN(n){var
e=j(ai),p=k===e?ai[1]:d===e?a(i[2],ai):ai,f=j(ah),q=k===f?ah[1]:d===f?a(i[2],ah):ah,g=j(ay),b=k===g?ay[1]:d===g?a(i[2],ay):ay;function
h(c,b){return a(o[s],[0,q,[0,c,b]])}function
l(c,b){return a(o[s],[0,p,[0,c,b]])}var
m=h(b,b);function
c(a){return typeof
a==="number"?b:0===a[0]?h(b,l(m,c(a[1]))):l(m,c(a[1]))}return c(n)}function
xd(e){var
b=a(l[29][3],e);if(0===b){var
c=j(ax);return k===c?ax[1]:d===c?a(i[2],ax):ax}return jN(a(l[30][2],b))}function
a1(b){if(typeof
b==="number"){if(0===b){var
c=j(ax);return k===c?ax[1]:d===c?a(i[2],ax):ax}var
e=j(ay);return k===e?ay[1]:d===e?a(i[2],ay):ay}else
switch(b[0]){case
0:var
f=j(bk),q=[0,eX(b[1])],r=k===f?bk[1]:d===f?a(i[2],bk):bk;return a(o[s],[0,r,q]);case
1:var
g=j(bj),t=[0,bz(b[1])],u=k===g?bj[1]:d===g?a(i[2],bj):bj;return a(o[s],[0,u,t]);case
2:var
v=b[1],w=a1(b[2]),h=j(ah),x=[0,a1(v),w],y=k===h?ah[1]:d===h?a(i[2],ah):ah;return a(o[s],[0,y,x]);case
3:var
z=b[1],A=a1(b[2]),l=j(aL),B=[0,a1(z),A],C=k===l?aL[1]:d===l?a(i[2],aL):aL;return a(o[s],[0,C,B]);case
4:var
D=b[1],E=a1(b[2]),m=j(ai),F=[0,a1(D),E],G=k===m?ai[1]:d===m?a(i[2],ai):ai;return a(o[s],[0,G,F]);case
5:var
n=j(bi),H=[0,a1(b[1])],I=k===n?bi[1]:d===n?a(i[2],bi):bi;return a(o[s],[0,I,H]);default:var
p=j(a0),J=[0,a1(b[1])],K=k===p?a0[1]:d===p?a(i[2],a0):a0;return a(o[s],[0,K,J])}}var
xe=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gr),c=j(cl);function
q(b){var
c=a(l[29][3],b);return eW(a(l[30][1],c))}var
r=k===c?cl[1]:d===c?a(i[2],cl):cl,e=j(ai),s=k===e?ai[1]:d===e?a(i[2],ai):ai,f=j(a0),t=k===f?a0[1]:d===f?a(i[2],a0):a0,h=j(aL),u=k===h?aL[1]:d===h?a(i[2],aL):aL,m=j(ah),v=k===m?ah[1]:d===m?a(i[2],ah):ah,n=j(bh),w=k===n?bh[1]:d===n?a(i[2],bh):bh;return[0,w,a1,v,u,t,s,r,q,p]}];function
xf(f,c){var
u=jM(c),v=a(au[21],u);function
x(b,a){return[0,a,b+1|0]}var
p=b(g[14],x,v),q=jL(c);function
y(c){var
d=f[1],e=b(m[4],xg,c[2]);return[0,a(I[69],e),d]}var
n=b(g[13],y,p);function
z(c,f){var
d=o[eb],e=b(m[4],xh,c+1|0);return[0,a(I[69],e),d]}var
r=b(g[14],z,q);function
A(b,a){return[0,a[1],b[1]]}var
B=e(g[19],A,p,n);function
t(e,c){function
d(c){switch(c[0]){case
0:return a(f[2],c[1]);case
1:var
h=a(l[29][2],c[1]),i=e+b(g[33],h,p)|0;return a(o[gT],i);case
2:var
j=c[1],k=d(c[2]),m=[0,d(j),k];return a(o[s],[0,f[3],m]);case
3:var
n=c[1],q=d(c[2]),r=[0,d(n),q];return a(o[s],[0,f[4],r]);case
4:var
t=c[1],u=d(c[2]),v=[0,d(t),u];return a(o[s],[0,f[6],v]);case
5:var
w=[0,d(c[1])];return a(o[s],[0,f[5],w]);default:var
x=c[1],y=a(f[8],c[2]),z=[0,d(x),y];return a(o[s],[0,f[7],z])}}return d(c)}function
C(l,e,c){try{var
p=[0,b(g[33],l,f[9]),[0,e,c]],q=a(o[s],p);return q}catch(b){b=w(b);if(b===O){var
h=j(az),m=[0,f[1],e,c],n=k===h?az[1]:d===h?a(i[2],az):az;return a(o[s],[0,n,m])}throw b}}function
h(f,e,c){if(typeof
c==="number"){if(0===c){var
l=j(bc);return k===l?bc[1]:d===l?a(i[2],bc):bc}var
m=j(av);return k===m?av[1]:d===m?a(i[2],av):av}else
switch(c[0]){case
0:var
x=f+b(bV[2],q,c[1])|0;return a(o[gT],x);case
1:var
g=c[1],u=g[2],v=g[1],w=t(e,g[3]);return C(u,t(e,v),w);case
2:var
y=c[1],z=h(f,e,c[2]),n=j(ba),A=[0,h(f,e,y),z],B=k===n?ba[1]:d===n?a(i[2],ba):ba;return a(o[s],[0,B,A]);case
3:var
D=c[1],E=h(f,e,c[2]),p=j(bb),F=[0,h(f,e,D),E],G=k===p?bb[1]:d===p?a(i[2],bb):bb;return a(o[s],[0,G,F]);case
4:var
r=j(av),H=c[1],I=k===r?av[1]:d===r?a(i[2],av):av,J=h(f,e,H);return b(o[49],J,I);default:var
K=c[1],L=h(f+1|0,e+1|0,c[3]),M=h(f,e,K);return b(o[49],M,L)}}var
D=a(g[1],n),E=a(g[1],r),F=a$(function(c){var
d=b(bV[2],q,c),e=b(m[4],xi,d),f=a(I[69],e);return a(o[bG],f)},c),G=a(g[6],B),H=a(g[6],r),J=h(a(g[1],n),0,c);function
K(a){return[0,[0,a[1]],a[2]]}var
L=b(g[13],K,n),M=e(o[63],D,L,J);function
N(a){return[0,[0,a[1]],a[2]]}var
P=b(g[13],N,r);return[0,e(o[63],E,P,M),H,G,F]}var
n=[0,i6,gk,i7,i8,i9,i_,aq,v,aK,ar,aY,aZ,ba,bb,c4,c5,bc,av,c6,c7,te,c8,c9,ti,c_,c$,da,tn,tp,tr,bd,be,bf,bg,db,dc,dd,de,bh,tC,aw,tF,df,dg,dh,di,dj,dk,dl,dm,dn,ax,ay,tS,tU,tW,tY,t0,i$,ja,jb,jc,az,cb,cc,cd,ce,cf,ua,uc,jd,je,jf,cg,ch,ci,cj,ck,jg,jh,ji,jj,ah,aL,a0,ai,uu,bi,cl,bj,bk,dp,dq,dr,ds,dt,du,dv,dw,dx,dy,dz,dA,dB,dC,dD,dE,dF,dG,dH,dI,dJ,dK,dL,uX,u1,u5,dM,dN,dO,dP,dQ,dR,dS,dT,vv,vz,vD,vH,dU,vO,vP,P,cm,gl,jk,eW,cn,aA,dV,jl,gm,vW,jm,vY,bU,bz,jn,v0,eX,dW,bl,bm,bn,jo,jp,v9,dV,aA,gn,go,gp,eY,wk,wn,wo,jq,jr,wC,wE,dX,gq,gr,gs,wF,gt,jt,ju,jv,wJ,jw,bV,eZ,jy,jz,jA,bU,dW,jB,jC,jD,jE,jF,e1,w8,w9,w_,bB,jG,jH,jI,jJ,dY,w$,xa,jL,jM,xb,xc,jN,xd,a1,xe,xf,function(f,e){var
c=e,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[3],i=d[2],j=a(I[1][6],d[1]),c=H(o[51],j,i,h,c),b=g;continue}return c}}];function
dZ(d){var
c=d;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:return[0,a(l[29][1],c[1]),0];case
2:var
c=c[2];continue;case
3:var
e=c[1],f=dZ(c[2]),g=dZ(e);return b(h[22],g,f);case
4:var
i=c[1],j=dZ(c[2]),k=dZ(i);return b(h[22],k,j)}return 0}}function
xj(a,f,e){return function(h){var
a=h;for(;;){if(a){var
d=a[1],i=a[2];try{var
j=b(g[5],e,d),k=b(g[5],f,d)===j?1:0,c=k}catch(a){a=w(a);if(a[1]!==jx)throw a;var
c=0}if(c){var
a=i;continue}return c}return 1}}(a)}function
xk(d,c,f){function
e(i,h){var
d=i,c=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
j=a(l[29][1],c[1]),k=b(g[5],f,j)[2];return b(I[1][10][7],d,k);case
2:var
c=c[2];continue;case
3:case
4:var
m=c[2],d=e(d,c[1]),c=m;continue}return d}}return e(d,c)}function
jO(e,d,a){if(a){var
g=a[2],c=b(l[6],e,[0,a[1],d]);if(c){var
h=c[1],f=jO(e,d,g);return f?[0,[0,h,f[1]]]:0}return 0}return xl}function
xm(e,d){var
c=e,b=d;for(;;){if(b){var
f=b[2],g=[0,c,[0,a(o[bG],b[1])]],c=a(o[s],g),b=f;continue}return c}}var
d0=[d,function(a){return e(E[6],xp,xo,xn)}],d1=[d,function(a){return e(E[6],xs,xr,xq)}],d2=[d,function(a){return e(E[6],xv,xu,xt)}],d3=[d,function(a){return e(E[6],xy,xx,xw)}];function
d4(c,b){if(typeof
b==="number"){var
e=j(d2),h=[0,c],l=k===e?d2[1]:d===e?a(i[2],d2):d2;return a(o[s],[0,l,h])}else{if(0===b[0]){var
f=j(d1),m=[0,c,b[1]],n=k===f?d1[1]:d===f?a(i[2],d1):d1;return a(o[s],[0,n,m])}var
p=b[2],q=b[1],r=d4(c,b[3]),g=j(d0),t=[0,c,d4(c,q),p,r],u=k===g?d0[1]:d===g?a(i[2],d0):d0;return a(o[s],[0,u,t])}}function
gu(b){if(b){var
c=b[1][1],d=0,f=function(d,b){var
e=b[1],f=a(l[30][2],b[2]);return H(t[88],c,f,e,d)};return e(g[16],f,d,b)}return 0}function
gv(c,a){return typeof
a==="number"?b(h[49],c,xz):0===a[0]?H(m[1],c,xA,n[aC],a[1]):bX(m[1],c,xB,gv,a[1],n[aC],a[2],gv,a[3])}function
e2(b){if(typeof
b==="number"){var
c=n[55],m=j(c);return k===m?c[1]:d===m?a(i[2],c):c}else
switch(b[0]){case
0:var
u=b[1],v=e2(b[2]),w=[0,e(n[bY],n[34],n[d_],u),v],f=n[56],p=j(f),x=k===p?f[1]:d===p?a(i[2],f):f;return a(o[s],[0,x,w]);case
1:var
y=b[1],z=e2(b[2]),A=[0,e(n[bY],n[34],n[d_],y),z],g=n[57],q=j(g),B=k===q?g[1]:d===q?a(i[2],g):g;return a(o[s],[0,B,A]);default:var
h=n[54],r=j(h),C=b[3],D=b[2],E=b[1],F=k===r?h[1]:d===r?a(i[2],h):h,G=e(n[kU],F,e2,C),H=e(n[bY],n[34],n[d_],D),I=[0,e(n[bY],n[34],n[d_],E),H,G],l=n[58],t=j(l),J=k===t?l[1]:d===t?a(i[2],l):l;return a(o[s],[0,J,I])}}function
bC(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:return 1;case
1:return 1;case
2:return 1+bC(a[2])|0;case
5:return 1;default:var
b=a[1],c=bC(a[2]);return bC(b)+c|0}}function
e3(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:var
b=a[2],c=bC(a[1]);return e3(b)+c|0;case
1:var
d=a[2],f=bC(a[1]);return e3(d)+f|0;default:var
h=a[3],i=a[2],j=a[1],k=0,l=function(b,a){return e3(a)+b|0},m=e(g[16],l,k,h),n=bC(i);return(bC(j)+n|0)+m|0}}function
jP(a){return e2(a)}function
aN(b,a){return D(m[1],b,xC,n[aC],a[1],n[140],a[2])}function
bW(d,c){if(typeof
c==="number")return b(m[1],d,xD);else
switch(c[0]){case
0:var
f=c[2],g=c[1],h=a(n[bq],n[aC]);return D(m[1],d,xE,h,g,bW,f);case
1:var
i=c[2],j=c[1],k=a(n[bq],n[aC]);return D(m[1],d,xF,k,j,bW,i);default:var
l=c[3],o=c[2],p=c[1],q=e(n[157],xH,xG,bW),r=a(n[bq],n[aC]),s=a(n[bq],n[aC]);return bX(m[1],d,xI,s,p,r,o,q,l)}}function
gw(h,g,f,e,b){if(b){var
i=b[1],m=i[2],o=i[1],c=gw(h,g,f,e,b[2]),j=c[3],k=c[2],l=c[1];try{var
d=aO(n[k5],h,g,k,j,m),p=[0,[0,[0,o,d[1]],l],d[2],d[3]];return p}catch(b){b=w(b);if(a(bv[21],b))return[0,l,k,j];throw b}}return[0,0,f,e]}function
gx(d,c,h,g,f){var
i=a(l[32][1],0),b=aO(n[k5],d,c,h,i,f),j=b[1],e=gw(d,c,b[2],b[3],g);return[0,e[1],j,e[2]]}var
d5=[d,function(q){var
b=n[54],f=j(b),l=k===f?b[1]:d===f?a(i[2],b):b,c=n[34],g=j(c),m=n[d_],o=k===g?c[1]:d===g?a(i[2],c):c,e=n[34],h=j(e),p=k===h?e[1]:d===h?a(i[2],e):e;return[0,p,o,m,l,jP]}],d6=[d,function(s){var
m=b(n[bY],n[38],n[d$]),c=n[e$],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[38],h=j(e),p=n[d$],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[38],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}],xJ=[d,function(s){var
m=b(n[bY],n[38],n[d$]),c=n[e$],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[42],h=j(e),p=n[ln],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[39],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}];function
jQ(d,c,a){return b(d,c,a)?1:b(d,a,c)?0:1}function
jR(d,c,a){function
e(b){return jQ(d,a,b)}return b(g[23],e,c)}function
jS(i,g){var
d=0,c=g;for(;;){if(c){var
f=c[2],e=c[1];if(a(i,e))return[0,[0,e],b(h[22],d,f)];var
d=[0,e,d],c=f;continue}return[0,0,d]}}function
jT(b,a){return jS(function(c){return jR(b,a,c)},a)}function
gy(a,d){var
b=jT(a,d),c=b[1];if(c){var
e=c[1];return[0,e,gy(a,b[2])]}return 0}function
xK(a){return gy(jK[45],a)}function
jU(c,q,p,m,l){var
f=n[kS],g=j(f),r=[0,c[2]],t=k===g?f[1]:d===g?a(i[2],f):f,h=a(o[s],[0,t,r]),u=b(n[kg],c[2],c[3]),v=e(n[lw],h,u,l),w=gu(m),x=d4(c[1],w),y=[0,function(l){function
m(a){return a}var
r=b(ae[48][3],m,l),u=a(ae[7],r),f=j(d3),t=0,w=[0,[0,xL,q,p],0],y=[0,c[1]],z=k===f?d3[1]:d===f?a(i[2],d3):d3,A=[0,[0,xM,x,a(o[s],[0,z,y])],w],e=n[ld],g=j(e),B=[0,h],C=k===g?e[1]:d===g?a(i[2],e):e,D=[0,[0,xN,v,a(o[s],[0,C,B])],A],E=b(n[kf],D,u),F=[0,a(aj[52],E),t];return a(L[70][20],F)}];return a(e4[63][9],y)}function
jV(d,c){function
e(b){var
c=b[1];return[0,function(d){var
e=[0,a(b[2],0),d],c=a(b[3],e);return c?[0,[0,c[1],b]]:0},c]}var
f=b(g[13],e,d);function
h(a){return a[1]}var
i=b(g[13],h,c);return b(l[6],f,i)}function
e5(e,a){function
b(a){if(a){var
f=a[2],c=jV(e,a[1]);if(c){var
g=c[1],d=b(f);return d?[0,[0,g,d[1]]]:0}return 0}return xO}return b(a)}function
xP(d,a,c){b(h[49],a,xQ);function
e(b){return H(m[1],a,xR,d,b)}b(g[11],e,c);return b(h[49],a,xS)}function
jW(f,d,c){function
i(k,c,j){var
d=c[2],m=c[1];function
n(b,a){return[0,b[1],a]}var
e=b(l[23],n,j);function
o(d){try{var
f=b(g[5],k,d)[1],c=f}catch(b){b=w(b);if(b[1]!==fk)throw b;var
c=a(h[2],xT)}return b(g[33],c,e)}try{var
t=b(d[5],m,o),i=t}catch(c){c=w(c);if(!a(bv[21],c))throw c;var
p=function(a){return a[1]},q=b(g[13],p,e),r=[0,a(d[2],0),q],f=a(d[3],r),s=f?f[1]:a(h[2],xU),i=s}return i}var
j=b(g[40],f,d);function
k(c){function
f(b){var
d=b[2],g=b[1],f=i5(a(d[2][4],d[1]),g);return e(l[11],V,f,c)}var
d=b(g[29],f,j);return i(d[1],d[2],c)}return b(g[13],k,c)}function
jX(C,c){function
e(c){if(typeof
c==="number")return 0===c?0:1;else
switch(c[0]){case
0:return[0,c[1]];case
1:var
w=c[3],x=c[2],D=c[1];return b(l[33][3],x,C)?[1,D,x,w]:[0,w];case
2:var
E=c[2],g=e(c[1]),h=e(E);if(typeof
g!=="number"&&0===g[0])if(typeof
h!=="number"&&0===h[0]){var
m=n[13],y=j(m),F=[0,g[1],h[1]],G=k===y?m[1]:d===y?a(i[2],m):m;return[0,a(o[s],[0,G,F])]}return[2,g,h];case
3:var
H=c[2],p=e(c[1]),q=e(H);if(typeof
p!=="number"&&0===p[0])if(typeof
q!=="number"&&0===q[0]){var
r=n[14],z=j(r),I=[0,p[1],q[1]],J=k===z?r[1]:d===z?a(i[2],r):r;return[0,a(o[s],[0,J,I])]}return[3,p,q];case
4:var
t=e(c[1]);if(typeof
t!=="number"&&0===t[0]){var
u=n[15],A=j(u),K=[0,t[1]],L=k===A?u[1]:d===A?a(i[2],u):u;return[0,a(o[s],[0,L,K])]}return[4,t];default:var
B=c[2],M=c[3],v=e(c[1]),f=e(M);if(typeof
v!=="number"&&0===v[0]){var
N=v[1];if(B)return f;if(typeof
f!=="number"&&0===f[0])return[0,b(o[49],N,f[1])]}return[5,v,B,f]}}return e(c)}function
bo(c,b){if(typeof
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
d=b[1],e=c[1],f=bo(c[2],b[2]);return[2,bo(e,d),f]}break;case
3:if(typeof
b!=="number"&&3===b[0]){var
g=b[1],i=c[1],j=bo(c[2],b[2]);return[3,bo(i,g),j]}break;case
4:if(typeof
b!=="number"&&4===b[0])return[4,bo(c[1],b[1])];break;default:if(typeof
b!=="number"&&5===b[0]){var
k=b[2],l=b[1],m=c[1],n=bo(c[3],b[3]);return[5,bo(m,l),k,n]}}return a(h[2],xV)}var
e6=[a5,xW,a4(0)];function
gz(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return e(g[17],d,b,c)}function
gA(k,j,i,h,f,t,D,s,r,C){var
m=gz(s,r)[1],o=eV(k,j,i,h,m),c=o[1],u=o[2],p=e5(t,c);if(p){var
q=p[1],v=b(g[40],c,q),w=l[33][1],x=function(c,a){return b(l[33][4],a,c)},y=e(g[16],x,w,u),z=function(f,c){var
d=c[2],h=c[1],i=l[33][1],j=a(d[2][4],d[1]);function
k(c,a){var
d=b(g[5],h,c)[2];return b(l[33][4],d,a)}var
m=e(au[15],k,j,i);return b(l[33][7],f,m)},d=jX(e(g[16],z,y,v),m),A=jW(c,q,eV(k,j,i,h,d)[1]),B=gj(d);return[0,[0,B,d,e(n[kU],f[4],f[5],A)]]}return 0}function
bp(aL,aK,aJ,aI,aH,p,m){return function(aM,aN){var
c=[0,function(G){function
J(a){return a}var
c=b(ae[48][3],J,G),K=a(ae[7],c),M=a(ae[10],c);try{var
q=gx(c,aL,n[fe][3],M,K),U=q[2],V=q[1],v=a(n[fe][4],q[3]),x=j(p),r=k===x?p[1]:d===x?a(i[2],p):p,y=j(m),W=k===y?m[1]:d===y?a(i[2],m):m,z=gA(aK,aJ,aI,aH,r,aM,v,V,U,c);if(z)var
t=z[1],A=t[2],X=t[3],Y=t[1],f=b(n[lt],W,A),u=f[3],Z=f[4],_=f[2],$=f[1],B=function(a){return b(aj[2],0,a[1])},aa=b(g[13],B,u),ab=a(L[70][20],aa),ac=b(g[13],B,_),ad=a(L[70][20],ac),af=function(a){return[0,[0,Q[4],[1,[0,a]]]]},ag=a(I[1][6],x3),D=e(aj[14],0,ag,c),ah=function(b){var
c=b[2];return[0,a(o[bG],b[1]),c]},ai=b(g[13],ah,u),l=n[21],E=j(l),ak=0,al=[0,r[4]],am=k===E?l[1]:d===E?a(i[2],l):l,an=[0,ad,[0,ab,[0,jU(r,X,a(o[s],[0,am,al]),ai,Z),ak]]],ao=a(L[70][20],an),ap=a(n[kA],A),aq=a(g[6],ap),ar=function(a){return b(g[5],v,a[2]-1|0)},as=b(g[13],ar,u),at=b(h[22],aq,as),au=b(L[70][3],ao,aN),av=a(aj[77],0),aw=b(L[70][3],av,au),ax=[0,a(o[bG],D),at],ay=a(o[59],ax),az=[0,a(aj[45],ay),0],aA=b(g[13],o[bG],Y),aB=[0,a(aj[aC],aA),az],aD=[0,aw,[0,a(L[70][20],aB),0]],aE=af(D),aF=H(aj[kv],1,x4,aE,$),F=b(L[70][19],aF,aD);else
var
aG=a(bA[3],x5),F=b(L[70][4],0,aG);return F}catch(c){c=w(c);if(c===n[k2]){var
N=a(bA[3],xX);return b(L[70][4],0,N)}if(c===C[23]){var
O=a(bA[3],xY);return b(L[70][4],0,O)}if(c===e6){a(h[46],h[24]);var
P=b(h[16],x0,xZ),R=b(h[16],x1,P),S=b(h[16],x2,R),T=a(bA[3],S);return b(L[70][4],0,T)}throw c}}];return a(e4[63][9],c)}}function
jY(z,y,x){var
c=n[42],m=j(c),p=k===m?c[1]:d===m?a(i[2],c):c,f=n[39],q=j(f),A=n[ln],r=k===q?f[1]:d===q?a(i[2],f):f,g=n[e$],t=j(g),B=k===t?g[1]:d===t?a(i[2],g):g,h=n[21],u=j(h),C=[0,B],D=k===u?h[1]:d===u?a(i[2],h):h,F=a(o[s],[0,D,C]),l=n[kS],v=j(l),G=[0,p],H=k===v?l[1]:d===v?a(i[2],l):l,w=a(o[s],[0,H,G]),I=b(n[kg],p,A),J=e(n[lw],w,I,x),K=d4(r,gu(y)),M=[0,function(g){function
h(a){return a}var
l=b(ae[48][3],h,g),p=a(ae[7],l),q=[0,e(E[6],x9,x8,x7),[0,r]],t=[0,[0,x_,K,a(o[s],q)],[0,[0,x6,z,F],0]],c=n[ld],f=j(c),m=0,u=[0,w],v=k===f?c[1]:d===f?a(i[2],c):c,x=[0,[0,x$,J,a(o[s],[0,v,u])],t],y=b(n[kf],x,p),A=[0,a(aj[52],y),m];return a(L[70][20],A)}];return a(e4[63][9],M)}function
d7(aM){return function(aN){var
G=n[197],J=t[s],K=t[119],M=t[121],N=t[122],f=[d,function(s){var
m=b(n[bY],n[38],n[d$]),c=n[e$],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[42],h=j(e),p=n[d$],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[39],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}],c=[0,function(O){function
P(a){return a}var
c=b(ae[48][3],P,O),R=a(ae[7],c),S=a(ae[10],c);try{var
p=gx(c,G,n[fe][3],S,R),s=p[2],u=p[1],v=a(n[fe][4],p[3]),x=j(f),Z=k===x?f[1]:d===x?a(i[2],f):f,_=function(b){var
c=b[2],d=b[1];return[0,d,aX(a(t[73],t[gV]),c)]},$=b(g[13],_,u),y=gA(J,K,M,N,Z,aM,v,$,aX(a(t[73],t[gV]),s),c);if(y)var
q=y[1],aa=q[3],ab=q[2],ac=q[1],ad=function(a){return b(g[27],a[1],ac)},z=gz(b(g[30],ad,u),s),af=z[2],A=bo(ab,z[1]),l=n[213],B=j(l),ag=k===B?l[1]:d===B?a(i[2],l):l,m=b(n[lt],ag,A),r=m[3],ah=m[4],ai=m[2],ak=m[1],D=function(a){return b(aj[2],0,a[1])},al=b(g[13],D,r),am=a(L[70][20],al),an=b(g[13],D,ai),ao=a(L[70][20],an),ap=function(a){return[0,[0,Q[4],[1,[0,a]]]]},aq=a(I[1][6],yg),E=e(aj[14],0,aq,c),ar=function(b){var
c=b[2];return[0,a(o[bG],b[1]),c]},as=[0,ao,[0,am,[0,jY(aa,b(g[13],ar,r),ah),0]]],at=a(L[70][20],as),au=a(n[kA],A),av=a(g[6],au),aw=function(a){return b(g[5],v,a[2]-1|0)},ax=b(g[13],aw,r),ay=b(h[22],av,ax),az=b(L[70][3],at,aN),aA=a(aj[77],0),aB=b(L[70][3],aA,az),aD=[0,a(o[bG],E),ay],aE=a(o[59],aD),aF=[0,a(aj[45],aE),0],aG=b(g[13],o[bG],af),aH=[0,a(aj[aC],aG),aF],aI=[0,aB,[0,a(L[70][20],aH),0]],aJ=ap(E),aK=H(aj[kv],1,yh,aJ,ak),F=b(L[70][19],aK,aI);else
var
aL=a(bA[3],yi),F=b(L[70][4],0,aL);return F}catch(c){c=w(c);if(c===n[k2]){var
T=a(bA[3],ya);return b(L[70][4],0,T)}if(c===C[23]){var
U=a(bA[3],yb);return b(L[70][4],0,U)}if(c===e6){a(h[46],h[24]);var
V=b(h[16],yd,yc),W=b(h[16],ye,V),X=b(h[16],yf,W),Y=a(bA[3],X);return b(L[70][4],0,Y)}throw c}}];return a(e4[63][9],c)}}function
jZ(d,c){var
b=a(d,c);return b?[0,[0,b[1],0]]:0}var
j0=a(c2[1],[0,V,br[20]]),gB=a(yk[8],yj)?0:[d,function(a){throw e6}];function
j2(o,n){var
f=j(gB);if(k!==f)if(d===f)a(i[2],gB);var
p=[0,yo,[0,yn,[0,b(h[16],ym,yl[30]),0]]],q=a(yp[3],0),m=e(g[16],yq[4],q,p),c=e(l[35],m,[0,m],[0,o,n]);return 0===c[0]?c[1]:a(h[2],c[1])}function
yr(a){return j2(a[1],a[2])}var
j3=b(j0[6],j1,yr);function
gC(c,b){return a(j3,[0,c,b])}function
d8(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,d8(a[2])];default:var
c=a[2],d=a[1],e=d8(a[3]);return[2,d8(d),c,e]}}function
gD(f,c){var
d=gC(f,c);if(d){var
e=a(aJ[51],d[1]);return b(t[111],c,e)?[0,e]:(a(h[27],ys),0)}return 0}function
j4(f,c){function
i(a){var
b=a[2];return[0,d8(a[1]),b]}var
d=gC(f,b(g[13],i,c));if(d){var
e=a(aJ[54],d[1]);return b(t[89],c,e)?[0,e]:(a(h[27],yt),a(h[46],h[24]),0)}return 0}function
co(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=a(l[29][1],c[1]);return e<=g?b(au[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],k=f(c[2],d),c=j,d=k;continue}return d}}return f(c,d)}function
cp(a){return co(0,au[1],a)}function
a2(b,d){function
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
e7(a){function
d(i,h,f){var
b=i,a=h,c=f;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=co(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=co(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=co(b,co(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return e(g[16],q,p,n)}}return d(0,a,au[1])}function
e8(c,f){function
d(c,b){return b<c?b:a(f,b-c|0)+c|0}function
e(c,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
f=a[1],g=e(c+1|0,a[2]);return[0,a2(f,function(a){return d(c,a)}),g];case
1:var
h=a[1],i=e(c+1|0,a[2]);return[1,a2(h,function(a){return d(c,a)}),i];default:var
j=a[3],k=a[2],l=a[1],m=function(a){return e(c+1|0,a)},n=b(t[10],m,j),o=a2(k,function(a){return d(c,a)});return[2,a2(l,function(a){return d(c,a)}),o,n]}}return e(0,c)}function
cq(d,c){function
e(b){var
c=b[2];return[0,a(t[71],b[1]),c]}return a(d,b(g[13],e,c))}var
gE=a(c2[1],[0,V,br[20]]),j5=a(c2[1],[0,V,br[20]]);function
yu(a){var
c=a[1],d=a[2];return cq(b(aJ[91],c[1],c[2]),d)}var
j6=b(gE[6],yv,yu);function
yw(a){var
c=a[1],d=a[2];return cq(b(aJ[92],c[1],c[2]),d)}var
j7=b(gE[6],yx,yw);function
yy(b){var
c=b[2];return cq(a(aJ[30],b[1]),c)}var
j8=b(j5[6],yz,yy);function
yA(b,a){return e(n[bF],aN,b,a[1])}var
yB=a(n[bq],aN),j9=[0,yC,eT,function(a){var
c=a[2];return cq(b(aJ[29],a[1],aJ[5]),c)},cp,a2,yB,yA];function
yD(b,a){return e(n[bF],aN,b,a[1])}var
yE=a(n[bq],aN),j_=[0,yF,eT,function(a){var
c=a[2];return cq(b(aJ[29],a[1],aJ[5]),c)},cp,a2,yE,yD];function
yG(b,a){return e(n[bF],aN,b,a[1])}var
gF=[0,yH,eT,j8,cp,a2,a(n[bq],aN),yG];function
gG(c,b){function
d(b,a){return e(n[bF],aN,b,a[1])}var
f=a(n[bq],aN);function
g(a){return gD(a[1],a[2])}return[0,yI,function(a){return[0,c,b]},g,cp,a2,f,d]}function
gH(c,b){function
d(b,a){return e(n[bF],aN,b,a[1])}var
f=a(n[bq],aN);function
g(a){return gD(a[1],a[2])}return[0,yJ,function(a){return[0,c,b]},g,cp,a2,f,d]}function
gI(b,a){function
c(b,a){return e(n[bF],n[aC],b,a[1])}function
d(a){var
b=a[2],c=a[1];return jZ(function(a){return j4(c,a)},b)}return[0,yK,function(c){return[0,b,a]},d,e7,e8,bW,c]}var
gJ=[0,yL,gh,j6,e7,e8,bW,function(b,a){return e(n[bF],n[aC],b,a[1])}],j$=[0,yM,gh,j7,e7,e8,bW,function(b,a){return e(n[bF],n[aC],b,a[1])}];function
yN(c){var
a=e5([0,gJ,0],eV(t[96],t[94],t[97],t[98],c)[1]);if(a){var
d=a[1],e=function(a){return a[1]};return[0,b(g[13],e,d)]}return 0}var
yO=a(bp(n[e_],t[eb],t[ff],t[fg],t[fj],d6,n[fd]),[0,j9,0]);function
yP(b){var
c=[0,gG(yQ,[0,b]),0];return a(bp(n[e_],t[eb],t[ff],t[fg],t[fj],d6,n[fd]),c)}var
yR=d7([0,j_,0]);function
yS(a){return d7([0,gH(yT,[0,a]),0])}function
yU(b){var
c=[0,gI(yV,[0,b]),0];return a(bp(n[fh],t[96],t[94],t[97],t[98],d5,n[fb]),c)}var
yX=[0,gI(yW,0),0],yY=a(bp(n[fh],t[96],t[94],t[97],t[98],d5,n[fb]),yX),y0=[0,gG(yZ,0),0],y1=a(bp(n[e_],t[eb],t[ff],t[fg],t[fj],d6,n[fd]),y0),y3=d7([0,gH(y2,0),0]),y4=a(bp(n[fh],t[96],t[94],t[97],t[98],d5,n[fb]),[0,gJ,0]),y5=a(bp(n[fh],t[96],t[94],t[97],t[98],d5,n[fb]),[0,j$,0]),y6=d7([0,gF,0]),_=[0,sA,sB,eR,gf,eS,gg,gh,eT,by,aX,a$,gj,eU,c3,sV,eV,au,sX,i5,n,dZ,xj,xk,jO,xm,d0,d1,d2,d3,d4,gu,gv,bC,e3,jP,aN,bW,gw,gx,d5,d6,xJ,jQ,jR,jS,jT,gy,xK,jU,jV,e5,e5,xP,jW,jX,bo,e6,gz,gA,bp,jY,d7,jZ,j0,j1,gB,j2,j3,gC,d8,gD,j4,co,cp,a2,e7,e8,cq,gE,j5,j6,j7,j8,j9,j_,gF,gG,gH,gI,gJ,j$,yN,yO,yP,yR,yS,yU,yY,y1,y3,y4,y5,y6,a(bp(n[e_],t[eb],t[ff],t[fg],t[fj],d6,n[fd]),[0,gF,0])];a3(771,_,"Micromega_plugin.Coq_micromega");a(as[12],y7);function
y8(d){var
b=[28,[0,0,[31,Q[4],[0,[0,y,y9],0],0]]],c=a(I[1][6],y_);return H(r[6][4],1,0,c,b)}var
y$=[0,function(b,a){return aj[54]}];e(r[6][9],0,[0,y,za],y$);b(as[19],y8,y);var
zb=0,zd=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(a){var
c=b(r[12][19],a,f);return b(_[97],-1,c)}}return a(h[2],zc)},zb],zf=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(U[6],cr[7]),i=b(r[12][2][7],g,f),j=a(U[6],r[1][1]),k=b(r[12][2][7],j,e);return function(a){var
c=b(r[12][19],a,k);return b(_[97],i,c)}}}return a(h[2],ze)},zd],zg=a(aB[12],zf);e(r[6][9],0,[0,y,zh],zg);function
zi(n){var
h=a(I[1][7],zj),b=r[1][1],f=0,g=0;if(0===b[0]){var
i=[0,[0,zl,[0,[1,Q[4],[5,[0,b[1]]],h],g]],f],k=a(I[1][7],zm),c=r[1][1],j=0;if(0===c[0]){var
l=[0,[1,Q[4],[5,[0,c[1]]],k],j],m=a(I[1][7],zo),d=cr[7];if(0===d[0])return e(r[9][4],[0,y,zr],0,[0,[0,zq,[0,[1,Q[4],[5,[0,d[1]]],m],l]],i]);throw[0,F,zp]}throw[0,F,zn]}throw[0,F,zk]}b(as[19],zi,y);var
zs=0,zu=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[101],d)}}return a(h[2],zt)},zs],zv=a(aB[12],zu);e(r[6][9],0,[0,y,zw],zv);function
zx(g){var
f=a(I[1][7],zy),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,zB],0,[0,[0,zA,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,zz]}b(as[19],zx,y);var
zC=0,zE=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[102],d)}}return a(h[2],zD)},zC],zF=a(aB[12],zE);e(r[6][9],0,[0,y,zG],zF);function
zH(g){var
f=a(I[1][7],zI),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,zL],0,[0,[0,zK,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,zJ]}b(as[19],zH,y);var
zM=0,zO=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[103],d)}}return a(h[2],zN)},zM],zP=a(aB[12],zO);e(r[6][9],0,[0,y,zQ],zP);function
zR(g){var
f=a(I[1][7],zS),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,zV],0,[0,[0,zU,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,zT]}b(as[19],zR,y);var
zW=0,zY=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[104],d)}}return a(h[2],zX)},zW],zZ=a(aB[12],zY);e(r[6][9],0,[0,y,z0],zZ);function
z1(g){var
f=a(I[1][7],z2),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,z5],0,[0,[0,z4,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,z3]}b(as[19],z1,y);var
z6=0,z8=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[98],d)}}return a(h[2],z7)},z6],z9=a(aB[12],z8);e(r[6][9],0,[0,y,z_],z9);function
z$(g){var
f=a(I[1][7],Aa),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,Ad],0,[0,[0,Ac,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,Ab]}b(as[19],z$,y);var
Ae=0,Ag=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[99],d)}}return a(h[2],Af)},Ae],Ah=a(aB[12],Ag);e(r[6][9],0,[0,y,Ai],Ah);function
Aj(g){var
f=a(I[1][7],Ak),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,An],0,[0,[0,Am,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,Al]}b(as[19],Aj,y);var
Ao=0,Aq=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[gR],d)}}return a(h[2],Ap)},Ao],Ar=a(aB[12],Aq);e(r[6][9],0,[0,y,As],Ar);function
At(g){var
f=a(I[1][7],Au),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,Ax],0,[0,[0,Aw,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,Av]}b(as[19],At,y);var
Ay=0,AA=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[93],d)}}return a(h[2],Az)},Ay],AB=a(aB[12],AA);e(r[6][9],0,[0,y,AC],AB);function
AD(g){var
f=a(I[1][7],AE),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,AH],0,[0,[0,AG,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,AF]}b(as[19],AD,y);var
AI=0,AK=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(c){var
d=b(r[12][19],c,f);return a(_[95],d)}}return a(h[2],AJ)},AI],AL=a(aB[12],AK);e(r[6][9],0,[0,y,AM],AL);function
AN(g){var
f=a(I[1][7],AO),b=r[1][1],c=0,d=0;if(0===b[0])return e(r[9][4],[0,y,AR],0,[0,[0,AQ,[0,[1,Q[4],[5,[0,b[1]]],f],d]],c]);throw[0,F,AP]}b(as[19],AN,y);var
AS=0,AU=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(a){var
c=b(r[12][19],a,f);return b(_[96],-1,c)}}return a(h[2],AT)},AS],AW=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(U[6],cr[7]),i=b(r[12][2][7],g,f),j=a(U[6],r[1][1]),k=b(r[12][2][7],j,e);return function(a){var
c=b(r[12][19],a,k);return b(_[96],i,c)}}}return a(h[2],AV)},AU],AX=a(aB[12],AW);e(r[6][9],0,[0,y,AY],AX);function
AZ(n){var
h=a(I[1][7],A0),b=r[1][1],f=0,g=0;if(0===b[0]){var
i=[0,[0,A2,[0,[1,Q[4],[5,[0,b[1]]],h],g]],f],k=a(I[1][7],A3),c=r[1][1],j=0;if(0===c[0]){var
l=[0,[1,Q[4],[5,[0,c[1]]],k],j],m=a(I[1][7],A5),d=cr[7];if(0===d[0])return e(r[9][4],[0,y,A8],0,[0,[0,A7,[0,[1,Q[4],[5,[0,d[1]]],m],l]],i]);throw[0,F,A6]}throw[0,F,A4]}throw[0,F,A1]}b(as[19],AZ,y);var
A9=0,A$=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(U[6],r[1][1]),f=b(r[12][2][7],e,d);return function(a){var
c=b(r[12][19],a,f);return b(_[94],-1,c)}}return a(h[2],A_)},A9],Bb=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(U[6],cr[7]),i=b(r[12][2][7],g,f),j=a(U[6],r[1][1]),k=b(r[12][2][7],j,e);return function(a){var
c=b(r[12][19],a,k);return b(_[94],i,c)}}}return a(h[2],Ba)},A$],Bc=a(aB[12],Bb);e(r[6][9],0,[0,y,Bd],Bc);function
Be(n){var
h=a(I[1][7],Bf),b=r[1][1],f=0,g=0;if(0===b[0]){var
i=[0,[0,Bh,[0,[1,Q[4],[5,[0,b[1]]],h],g]],f],k=a(I[1][7],Bi),c=r[1][1],j=0;if(0===c[0]){var
l=[0,[1,Q[4],[5,[0,c[1]]],k],j],m=a(I[1][7],Bk),d=cr[7];if(0===d[0])return e(r[9][4],[0,y,Bn],0,[0,[0,Bm,[0,[1,Q[4],[5,[0,d[1]]],m],l]],i]);throw[0,F,Bl]}throw[0,F,Bj]}throw[0,F,Bg]}b(as[19],Be,y);var
ka=[0,y];a3(777,ka,"Micromega_plugin.G_micromega");a3(778,[0,g0,l,t,p,C,aJ,c2,_,ka],"Micromega_plugin");return});
