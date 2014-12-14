:-use_module(library(clpfd)).
:-use_module(library(lists)).

%manage(+List of sales objetive per period,+List of factory production limits,+Time each product consumes,+Time each factory has,+Cost of each product in each factory,-List with required production)
manage(Products,Factories,CombFactories,HoursProducts,HoursFactory,CostProducts,ProductsPerFactory):-
        initialize(Products,StockI,SalesObj1,SalesObj2,SalesObj3),
        initProducts(Products,Factories,ProductsPerFactory1,Armazem1),
        calcPeriodo(ProductsPerFactory1,ProductsByType1,SalesObj1,StockI,Armazem1,Factories,CombFactories,HoursProducts,HoursFactory,CostProducts,Cost1),
        initProducts(Products,Factories,ProductsPerFactory2,Armazem2),
        calcPeriodo(ProductsPerFactory2,ProductsByType2,SalesObj2,Armazem1,Armazem2,Factories,CombFactories,HoursProducts,HoursFactory,CostProducts,Cost2),
        initProducts(Products,Factories,ProductsPerFactory3,Armazem3),
        addRestriction(CombFactories,CombFactories1),                                                             
        calcPeriodo(ProductsPerFactory3,ProductsByType3,SalesObj3,Armazem2,Armazem3,Factories,CombFactories1,HoursProducts,HoursFactory,CostProducts,Cost3),
        append(ProductsPerFactory1,ProductsPerFactory2,ProductsTemp),
        append(ProductsTemp,ProductsPerFactory3,ProductsPerFactory),
        append(ProductsPerFactory,ProductsLabel),
        Cost #= Cost1+Cost2+Cost3,
       reset_timer,labeling([ffc,bisect,minimize(Cost),time_out(60000,_F)],ProductsLabel),print_time,nl,write('Total Cost:   '),write(Cost),nl,
        showPeriod(ProductsByType1,1),nl,showPeriod(ProductsByType2,2),nl,showPeriod(ProductsByType3,3),nl,nl,
        write('Warehouse1:   '),write(Armazem1),nl,write('Warehouse2:   '),write(Armazem2),nl,write('Warehouse3:   '),write(Armazem3),nl.


reset_timer :- statistics(walltime,_).  
print_time :-
        statistics(walltime,[_,T]),
        TS is ((T//10)*10)/1000,
        nl, write('Time: '), write(TS), write('s'), nl, nl.

%showPeriod(+Products organized by some criteria,+Period to show)
showPeriod(Products,Count):-
        write('Period '),write(Count), write('-------------'),nl,nl,
        showProductsByType(Products,1).

%showProductsByFactory(+Products,+Factory)
showProductsByFactory([],_).
showProductsByFactory([X|Xs],Count):-
        write('Factory '),write(Count), write('       '),write(X),nl,
        Count1 is Count + 1,
        showProductsByFactory(Xs,Count1).

%showProductsByType(Products,IDProduct)
showProductsByType([],_).
showProductsByType([X|Xs],Count):-
        write('Product '),write(Count), write('       '),write(X),nl,
        Count1 is Count + 1,
        showProductsByType(Xs,Count1).

%calcPeriodo(-ProductsPerFactory,-ProductsByType,+SalesObj,+Stock,-Armazem,+Factories,+CombFactories,+HoursProducts,+HoursFactory,+CostProducts,-Cost)
calcPeriodo(ProductsPerFactory,ProductsByType,SalesObj,Stock,Armazem,Factories,CombFactories,HoursProducts,HoursFactory,CostProducts,Cost):-
        transpose(ProductsPerFactory,ProductsByType),
        calcProduction(ProductsByType,SalesObj,Stock,Armazem),
        factoryConstraints(ProductsPerFactory,Factories,HoursProducts,HoursFactory),
        combinationPerFactory(ProductsPerFactory,CombFactories),
        calcCostProduction(ProductsPerFactory,CostProducts,CostProd),
        calcCostArmazem(Armazem,CostArm),
        Cost #= CostArm + CostProd.

%addRestriction(+CombFactories,-NewCombFactories)
addRestriction([],[]).
addRestriction([CombFactories|Xs],[NewComb|Ys]):-
        append(CombFactories,[0,0,1],NewComb),
        addRestriction(Xs,Ys).

%initialize(+Products,-StockI,-SalesObj1,-SalesObj2,-SalesObj3)
initialize(Products,StockI,SalesObj1,SalesObj2,SalesObj3):-
        length(Products,NumProducts),
        length(StockI,NumProducts),
        fill(StockI,1),
        transpose(Products,[SalesObj1,SalesObj2,SalesObj3]).

%initProducts(+Products,+Factories,-ProductsPerFactory,-Armazem)
initProducts(Products,Factories,ProductsPerFactory,Armazem):-
        length(Products,NumProducts),
        length(Factories,NumFactories),
        length(Armazem,NumProducts),
        domain(Armazem,0,sup),
        length(ProductsPerFactory,NumFactories),
        createList(ProductsPerFactory,NumProducts).



%fill(-List,+Num)
fill([],_Num).
fill([Num|Xs],Num):-
        fill(Xs,Num).

%createList(-List,+Num)
createList([],_).
createList([Products|Xs],Num):-
                length(Products,Num),
                domain(Products,0,sup),
                createList(Xs,Num).

%calcProduction(-Products,+SalesObj,+Stock,-Armaz)
calcProduction([],[],[],[]).
calcProduction([Products|Xs],[SalesObj|Ys],[Stock|Zs],[Armaz|Res]):-
        Val #= SalesObj+2-Stock,
        sumList(Products,Num),
        Num #>= Val,
        Val1 #= SalesObj-Stock,
        Armaz #= Num - Val1,
        calcProduction(Xs,Ys,Zs,Res).

%sumList(+List,-Result)
sumList([],0).
sumList([X|Xs], Res):-
        sumList(Xs,Res1),
        Res #= Res1+X.

%sumLimitHours(+Products,+HourCost,-Sum,-SumHours)
sumLimitHours([],[],0,0).
sumLimitHours([X|Xs],[HourProd|Ys], NumLimit, NumHour):-
        sumLimitHours(Xs,Ys,NumLimit1,NumHour1),
        NumLimit #= X+NumLimit1,
        NumHour #= X*HourProd + NumHour1.

%factoryConstraints(-Products,+Limits,+Hours,+HourFac)
factoryConstraints([],[],_,[]).
factoryConstraints([Products|Xs],[Limit|Ys],Hours,[HourFac|Zs]):-
        sumLimitHours(Products,Hours,NumProd,NumHours),
        NumProd #=< Limit,
        NumHours #=< HourFac,
        factoryConstraints(Xs,Ys,Hours,Zs).

%combinationPerFactory(-Products,+CombFactories)
combinationPerFactory([],[]).
combinationPerFactory([Products|Xs],[CombFactories|Ys]):-
                             factoryCombinationConstraints(Products,CombFactories),
                             combinationPerFactory(Xs,Ys).

%factoryCombinationConstraint(-Products,+CombFactories)
factoryCombinationConstraints(_Products,[]).
factoryCombinationConstraints(Products,[IDProduct,MultFactor,ProductComparison|Ys]):-
        factoryCombination(Products,[IDProduct,MultFactor,ProductComparison],0,_,0),
        factoryCombinationConstraints(Products,Ys).

%factoryCombination(-Products,+Comb,+ID,-PrevProduct,+Found)
factoryCombination([Products|_Xs],[IDProduct,0,_ProductComparison],IDProduct,_,_):-
        Products #= 0.

factoryCombination([Products|Xs],[IDProduct,MultFactor,ProductComparison],IDProduct,SaveProduct,0):-
        Count #= IDProduct +1,
        SaveProduct #= Products,
        factoryCombination(Xs,[IDProduct,MultFactor,ProductComparison],Count,SaveProduct,1).

factoryCombination([Products|_Xs],[IDProduct,MultFactor,_ProductComparison],IDProduct,SaveProduct,1):-
        Products #= SaveProduct * MultFactor.
        
factoryCombination([Products|Xs],[IDProduct,MultFactor,ProductComparison],ProductComparison,SaveProduct,0):-
        Count #= ProductComparison +1,
        SaveProduct #= Products,
        factoryCombination(Xs,[IDProduct,MultFactor,ProductComparison],Count,SaveProduct,1).

factoryCombination([_Products|_Xs],[_IDProduct,0,ProductComparison],ProductComparison,SaveProduct,1):-
        SaveProduct #= 0.

factoryCombination([Products|_Xs],[_IDProduct,MultFactor,ProductComparison],ProductComparison,SaveProduct,1):-
        SaveProduct #= Products * MultFactor.

factoryCombination([_Products|Xs],[IDProduct,MultFactor,ProductComparison],Count,SaveProduct,Found):-
        IDProduct \= Count,
        ProductComparison \= Count,
        Count1 #= Count + 1,
        factoryCombination(Xs,[IDProduct,MultFactor,ProductComparison],Count1,SaveProduct,Found).
        
%calcCostFact(-Products,+Cost,-Res)
calcCostFact([],[],0).
calcCostFact([Produto|Xs],[Custo|Ys],Res):-
        calcCostFact(Xs,Ys,ResL),
        Res #= (Custo*Produto) + ResL.

%calcCostProduction(-Products,+Cost,-Res)
calcCostProduction([],[],0).
calcCostProduction([Produtos|Xs],[CustoL|Ys],Custo):-
        calcCostFact(Produtos,CustoL,Res1),
        calcCostProduction(Xs,Ys,Res2),
        Custo #= Res1 + Res2.

%calcCostArmazem(-Armazem,-Cost)
calcCostArmazem([],0).
calcCostArmazem([Armazem1|Xs],CostArm):-
        calcCostArmazem(Xs,CostArm1),
        CostArm #= Armazem1*5 + CostArm1.
        