module ReinforcementOnPostedOffer

import Random
import Distributions
import DataFrames
import Statistics

const Dist = Distributions
const Df = DataFrames
const Stats = Statistics

mutable struct Params
    numsellers::Int64
    numbuyers::Int64

    maxcost::Float64
    maxval::Float64
    
    numtrymax::Int64
end 

mutable struct Seller
    Id::Int64
    cost::Float64
    possibleMarkups::Array{Float64, 1}
    currMarkup_idx::Int64
    propensities::Array{Float64, 1}
    numTimesChosenMarkup::Array{Int64, 1}
    profits::Array{Float64, 1}
    soldout::Bool
end

mutable struct Buyer
    Id::Int64
    val::Float64
    numtry::Int64
    sellersVisited::Array{Seller, 1}
    minPricePaid::Union{Float64, Missing}
end 

function initparams(;numsellers=100, numbuyers=100, maxcost=200, maxval=200, numtrymax=5)

	return Params(numsellers, numbuyers, maxcost, maxval, numtrymax)
end

function create_sellers(params)
    sellers = Array{Seller, 1}()
    costs = rand(Dist.Uniform(10.0, params.maxcost), params.numsellers)

    for i = 1:params.numsellers
        cost = costs[i]
        possibleMarkups = [x for x = 0.25:0.25:10.0]
        currMarkup_idx = -1
        props = [0.0 for x = 1:40]
        timesChosenMarkups = [0 for x = 1:40]
        profits = [0.0 for x = 1:40]

        push!(sellers, Seller(i, cost, possibleMarkups, currMarkup_idx, props, timesChosenMarkups, profits, false)) 
    end 
   
    return sellers
end

function create_buyers(params)
   buyers = Array{Buyer, 1}()
   vals = rand(Dist.Uniform(0, params.maxval), params.numbuyers)

   for i = 1:params.numbuyers
       val = vals[i]
       numtry = rand(1:params.numtrymax)
       push!(buyers, Buyer(i, val, numtry, Array{Seller, 1}(), missing))
   end 
   
   return buyers
end

function reset_buyer_fields!(buyers)
    for buyer in buyers
        buyer.sellersVisited = Array{Seller, 1}()
    end 
end

function reset_seller_fields!(sellers)
    for seller in sellers
        seller.soldout = false
    end
end

function chooseMarkup(e, seller)
    if rand() < (1-e)
        maxPropensity = -Inf

        for i = 1:length(seller.propensities)
            if seller.propensities[i] > maxPropensity
                maxPropensity = seller.propensities[i]
            end   
        end 

        storeIdxForMarkups = Array{Int64, 1}()
        for i = 1:length(seller.propensities)
            if seller.propensities[i] == maxPropensity
                push!(storeIdxForMarkups, i)
            end 
        end 

        return rand(storeIdxForMarkups)
    else
        index = -1
        markup = rand(seller.possibleMarkups)
        for i = 1:length(seller.possibleMarkups)
            if markup == seller.possibleMarkups[i]
                index = i
            end
        end
        return index
    end
end

function updatePropensity(chosenMarkupIdx, seller)
    seller.propensities[chosenMarkupIdx] = (seller.propensities[chosenMarkupIdx] * (seller.numTimesChosenMarkup[chosenMarkupIdx]/(seller.numTimesChosenMarkup[chosenMarkupIdx]+1))) + (seller.profits[chosenMarkupIdx]*(1/(seller.numTimesChosenMarkup[chosenMarkupIdx]+1)))
    
    seller.numTimesChosenMarkup[chosenMarkupIdx] += 1    
end

function should_exec_trade_this_seller(buyer, seller, chosenMarkupIdx)
    if ismissing(buyer.minPricePaid)
        return false
    end

    if seller.cost * (1 + seller.possibleMarkups[chosenMarkupIdx]) <= buyer.minPricePaid
        return true
    else 
        return false
    end
end

function should_exec_trade_visited_sellers(buyer, chosenMarkupIdx)
    if length(buyer.sellersVisited) < buyer.numtry
        return false
    end 

    sellersWithQ = [seller for seller in buyer.sellersVisited if !seller.soldout]

    prices = [seller.cost * (1 + seller.possibleMarkups[chosenMarkupIdx]) for seller in sellersWithQ]

    if length(prices) == 0
        return false
    end 

    minprice = minimum(prices)
    if minprice <= buyer.val
        return true
    else 
        return false
    end 
end     

function exec_trade!(buyer, seller, chosenMarkupIdx, params)
    price = seller.cost * (1 + seller.possibleMarkups[chosenMarkupIdx])

    if ismissing(buyer.minPricePaid)
        buyer.minPricePaid = price 
    elseif buyer.minPricePaid > price
        buyer.minPricePaid = price
    end 

    profit = price - seller.cost
    seller.profits[chosenMarkupIdx] = profit 

    seller.soldout = true

    updatePropensity(chosenMarkupIdx, seller)

    return (cost = seller.cost, val = buyer.val, price = price)
end 

function exec_trade_visited_sellers(buyer, chosenMarkupIdx, params)
    sellersWithQ = [seller for seller in buyer.sellersVisited if !seller.soldout]

    prices = [seller.cost * (1 + seller.possibleMarkups[chosenMarkupIdx]) for seller in sellersWithQ]

    minprice, idx = findmin(prices)
    tradePartner = sellersWithQ[idx]

    cost, val, price = exec_trade!(buyer, tradePartner, chosenMarkupIdx, params)

    return (cost = cost, val = val, price = price)
end

function create_traders(params, rseedTraders)
    Random.seed!(rseedTraders)
    sellers = create_sellers(params)
    buyers = create_buyers(params)

    return sellers, buyers
end

function simulateMarket(rseed, e, sellers, buyers, bp, maxtimepd)
    Random.seed!(rseed)

    trades = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], sellercost=Float64[], buyerval=Float64[], price=Float64[])

    sellersdf = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], sellerid=Int64[], cost=Float64[], begend=String[], soldout=Bool[], markup=Float64[], curr_prop=Float64[], numTimes=Int64[], profits=Float64[])

    buyersdf = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], buyerid=Int64[], val=Float64[], numSellersVisited=Int64[], price=Float64[])

    for t = 1:maxtimepd
        reset_seller_fields!(sellers)
        reset_buyer_fields!(buyers)

        for seller in sellers
            chosenMarkupIdx = chooseMarkup(e, seller)
            seller.currMarkup_idx = chosenMarkupIdx
            push!(sellersdf, [rseed, t, e, seller.Id, seller.cost, "beg", seller.soldout, seller.possibleMarkups[chosenMarkupIdx], seller.propensities[chosenMarkupIdx], seller.numTimesChosenMarkup[seller.currMarkup_idx], seller.profits[seller.currMarkup_idx]]) 
        end 

        Random.shuffle!(buyers)
        for buyer in buyers
            Random.shuffle!(sellers)
            buyerpaid = -1.0
            for seller in sellers
                push!(buyer.sellersVisited, seller)

                if should_exec_trade_this_seller(buyer, seller, seller.currMarkup_idx)
                    cost, val, price = exec_trade!(buyer, seller, seller.currMarkup_idx, bp)
                    push!(trades, [rseed, t, e, cost, val, price])
                    buyerpaid = price
                    break
                elseif should_exec_trade_visited_sellers(buyer, seller.currMarkup_idx)
                    cost, val, price = exec_trade_visited_sellers(buyer, seller.currMarkup_idx, bp)
                    push!(trades, [rseed, t, e, cost, val, price])
                    buyerpaid = price
                    break
                end
            end

            buyersdf = push!(buyersdf, [rseed, t, e, buyer.Id, buyer.val, length(buyer.sellersVisited), buyerpaid]) 
        end 

        for seller in sellers
            push!(sellersdf, [rseed, t, e, seller.Id, seller.cost, "end", seller.soldout, seller.possibleMarkups[seller.currMarkup_idx], seller.propensities[seller.currMarkup_idx], seller.numTimesChosenMarkup[seller.currMarkup_idx], seller.profits[seller.currMarkup_idx]])
        end 
    end

    return trades, sellersdf, buyersdf  
end 

function eqm_price_quantity_surplus(sellers, buyers)
    costs = [seller.cost for seller in sellers]
    sort!(costs)

    vals = [buyer.val for buyer in buyers]
    sort!(vals, rev=true)

    numsellers = length(costs)
    numbuyers = length(vals)

    minlength = min(numsellers, numbuyers)

    posSurplusVals = [(vals[i] - costs[i]) for i = 1:minlength if vals[i] - costs[i] > 0]
    eqmqty = length(posSurplusVals)
    eqmprice = vals[eqmqty]
    surplus = sum(posSurplusVals)

    return (eqmprice = eqmprice, eqmqty = eqmqty, surplus = surplus)
end 

function calc_mean_surplus(trades, timepd)
    tradesSubset = trades[trades.time .== timepd, :]
    aggsurplusDf = Df.by(tradesSubset, :rseed, aggsurplus = :surplus => sum)

    return Stats.mean(aggsurplusDf.aggsurplus)
end 

function calc_mean_price(trades, timepd)
    tradesSubset = trades[trades.time .== timepd, :]
    avgpricesDf = Df.by(tradesSubset, :rseed, avgprice = :price => Stats.mean)

    return Stats.mean(avgpricesDf.avgprice)
end 

function calc_mean_qty(trades, timepd)
    tradesSubset = trades[trades.time .== timepd, :]
    qtyDf = Df.by(tradesSubset, :rseed, quantity = :price => length)

    return Stats.mean(qtyDf.quantity)
end 

function runSimulations(e)
    bp = initparams()
    sellers, buyers = create_traders(bp, 1)
    eqmprice, eqmqty, eqmsurplus = eqm_price_quantity_surplus(sellers, buyers)

    trades = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], sellercost=Float64[], buyerval=Float64[], price=Float64[])
    sellersdf = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], sellerid=Int64[], cost=Float64[], begend=String[], soldout=Bool[], markup=Float64[], curr_prop=Float64[], numTimes=Int64[], profits=Float64[])
    buyersdf = Df.DataFrame(rseed=Int64[], time=Int64[], e=Float64[], buyerid=Int64[], val=Float64[], numSellersVisited=Int64[], price=Float64[])

    for rseed = 101:200
        sellersmkt, buyersmkt = create_traders(bp, 1)
        mkttrades, mktsellersdf, mktbuyersdf = simulateMarket(rseed, e, sellersmkt, buyersmkt, bp, 500)
        trades = vcat(trades, mkttrades)
        sellersdf = vcat(sellersdf, mktsellersdf)
        buyersdf = vcat(buyersdf, mktbuyersdf)
        println("Finished simulation for random seed $rseed.")
    end 

    trades.surplus = trades.buyerval .- trades.sellercost

    return trades, sellersdf, buyersdf, eqmprice, eqmqty, eqmsurplus
end 

function summary_stats(e)
    trades, sellersdf, buyersdf, eqmprice, eqmqty, eqmsurplus = runSimulations(e)

    dfout = Df.DataFrame(time=Int64[], e=Float64[], surplus=Float64[], perceff=Float64[], price=Float64[], quantity=Int64[])

    push!(dfout, [0, e, eqmsurplus, 1.0, eqmprice, eqmqty])

    timepds = [1, 10, 50, 100, 200, 500]
    surplusVals = [calc_mean_surplus(trades, t) for t in timepds]
    perceff = [surplus/eqmsurplus for surplus in surplusVals]
    prices = [calc_mean_price(trades, t) for t in timepds]
    qtys = [calc_mean_qty(trades, t) for t in timepds]
    es = [e for x = 1:length(prices)] 

    dftemp = Df.DataFrame(time=timepds, e=es, surplus=surplusVals, perceff=perceff, price=prices, quantity=qtys)

    dfout = vcat(dfout, dftemp)

    println("Equilibrium surplus: $eqmsurplus")
    for (idx, t) in enumerate(timepds)
        println("Efficiency in timepd $t = $(perceff[idx])")
    end
    println("-"^80)

    println("Equilibrium price: $eqmprice")
    for t in timepds
        println("Average price in timepd $t = $(calc_mean_price(trades, t))")
    end 
    println("-"^80)

    println("Equilibrium quantity: $eqmqty")
    for t in timepds
        println("Average quantity in timepd $t = $(calc_mean_qty(trades, t))")
    end 
    println("-"^80)

    return trades, sellersdf, buyersdf, eqmprice, eqmqty, eqmsurplus, dfout
end 

function runDiffEValues()
    print("Enter an 'e' value from, eg. 0.0, 0.01, 0.1, 0.2: ")
    n = readline()
    
    return summary_stats(tryparse(Float64, n))
end 

end 