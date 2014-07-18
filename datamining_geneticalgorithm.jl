#= Mining and evolution functions =#

function init()
  #parametric constants
  global const MSK_2BITS          = 3

  global const GROUPS             = 6

  global const PATTERN_GROUP_SIZE = 2
  global const PATTERN_SIZE       = GROUPS * PATTERN_GROUP_SIZE

  global const RULE_GROUP_SIZE    = 4
  global const RULE_SIZE          = GROUPS * RULE_GROUP_SIZE

  global const MSK_FULLBITS       = (~0 >>> (sizeof(Int) * 8 - RULE_SIZE))
  global const MAX_RULE           = MSK_FULLBITS

  global const EQUALS_TO        = 0b11
  global const GREATER_THAN     = 0b10
  global const LESS_THAN        = 0b01
  global const DIFFERENT_FROM   = 0b00

  global loadeddata             = Dict{Uint,Uint}()

  global const attaspect        = [ "Interesse e compromisso", "Assiduidade e pontualidade", "Iniciativa", "Respeito às normas", "Cordialidade", "Desempenho final"]
  global const attconcept       = [ "Regular ou Insuficiente", "Bom", "Muito bom", "Ótimo" ]
  global const performance      = [ "D", "C", "B", "A" ]
end

function load()

  max::Uint = 2 ^ PATTERN_SIZE

  #cd("YOUR_FILE_DIRECTORY")!
  cd("C:/Users/Luis/Documents/gece/gece_dm_attitudinal")
  rawdata = readcsv("attitudinal_extracted_treated_data.csv", Uint)
  patterns = Array(Uint,max)
  fill!(patterns,0)

  #calculates the range of each of distinct patterns
  for i::Uint in rawdata
    patterns[i+1] += 1 #([i+1]) 1-based for arrays and also acts as differentiator for nulls patterns
  end

  #compute the total of distinct patterns
  total::Uint = 0
  for pattern in patterns
    if pattern != 0
      total += 1
    end
  end

  #optimization hint
  sizehint(loadeddata,total)

  #transfers the valid patterns from the arrangement for the dictionary
  globaltotal::Uint = 0
  for i = 1:max
    if patterns[i] != 0
      get!(loadeddata,i-1,patterns[i]) #(i-1) go back to 0-based
      globaltotal += patterns[i]
    end
  end

  #returns the total of distinct patterns and the global total of patterns
  return (total,globaltotal)
end

#chromosome efficiency
function objective(d::Dict{Uint,Uint}, r::Uint) #d:data patterns to be analized, r:rule to be applied

  reach::Uint = 0 #objective's reach

  for t in d #t:tuple with (key:pattern in the data group, value:amount of data represented)

    match::Bool       = true #rule matches to pattern (or not!)
    mskattpatt::Uint  = (MSK_2BITS << (PATTERN_SIZE - PATTERN_GROUP_SIZE)) #mask for attitudinal pattern block segment
    mskattrule::Uint  = (MSK_2BITS << (RULE_SIZE - RULE_GROUP_SIZE)) #mask for rule block segment
    mskattpersp::Uint = (MSK_2BITS << (RULE_SIZE - RULE_GROUP_SIZE + 2)) #mask for perspective operator

    offsetpatt::Uint  = PATTERN_GROUP_SIZE #offset for pattern
    offsetrule::Uint  = RULE_GROUP_SIZE #offset for rule

    for b::Uint in 1:GROUPS

      perspective::Uint = ((r & mskattpersp) >>> (RULE_SIZE - offsetrule + 2))
      ruleaspect::Uint = ((r & mskattrule) >>> (RULE_SIZE - offsetrule ))
      patternaspect::Uint = ((t[1] & mskattpatt) >>> (PATTERN_SIZE - offsetpatt))

#= Debug
      #$(lpad(XXXX,10," "))
      print("* ( $(lpad(r,10," ")) & $(lpad(mskattpersp,10," ")) ) >>> ")
      print("$(lpad(RULE_SIZE,10," ")) - $(lpad(offsetrule,10," ")) + 2)")
      println(" = $(perspective)")
=#

      #"different from" perspective
      if perspective == DIFFERENT_FROM
        if !(patternaspect != ruleaspect)
          match = false
          break
        end

      #"less than" perspective
      elseif perspective == LESS_THAN
        if !(patternaspect < ruleaspect)
          match = false
          break
        end

      #"greater than" perspective
      elseif perspective == GREATER_THAN
        if !(patternaspect > ruleaspect)
          match = false
          break
        end

      #"equals to" perspective
      else
        if !(patternaspect == ruleaspect)
          match = false
          break
        end

      end

      #next group
      mskattpatt >>= PATTERN_GROUP_SIZE
      mskattrule >>= RULE_GROUP_SIZE
      mskattpersp >>= RULE_GROUP_SIZE

      offsetrule += RULE_GROUP_SIZE
      offsetpatt += PATTERN_GROUP_SIZE

    end
  end

  return reach
end

function ruleinterpreter(r::Uint)
  #mask for attitudinal rule block segment
  mskattrule::Uint = (MSK_2BITS << (RULE_SIZE - RULE_GROUP_SIZE))
  #mask for attitudinal perspective operator
  mskattpersp::Uint = (MSK_2BITS << (RULE_SIZE - RULE_GROUP_SIZE + 2))

  print("\n> Rule ")

  for p::Uint in 1:RULE_GROUP_SIZE:RULE_SIZE
    i = (sizeof(Int) * 8) - RULE_SIZE + p
    j = i + RULE_GROUP_SIZE - 1
    print(bits(r)[i:i+1],".")
    print(bits(r)[i+2:j],(p < RULE_SIZE - RULE_GROUP_SIZE ? "|": ""))
  end
  println()

  offsetrule::Uint = RULE_GROUP_SIZE
  for i::Uint in 1:GROUPS

    perspective::Uint = ((r & mskattpersp) >>> (RULE_SIZE - offsetrule + 2))
    ruleaspect::Uint = ((r & mskattrule) >>> (RULE_SIZE - offsetrule ))

    #Attitudinal aspect
    print(" - \"", rpad(attaspect[i],30,"."), "\" ")

    #Attitudinal perspective

    if perspective == EQUALS_TO
      print("= ")
    elseif perspective == GREATER_THAN
      print("> ")
    elseif perspective == LESS_THAN
      print("< ")
    else #DIFFERENT_FROM
      print("! ")
    end

    if i::Uint < GROUPS
      print("\"", attconcept[ruleaspect + 1], "\"")
    else
      print("\"", performance[ruleaspect + 1], "\"")
    end

    #next group
    mskattrule >>= RULE_GROUP_SIZE
    mskattpersp >>= RULE_GROUP_SIZE
    offsetrule += RULE_GROUP_SIZE

    println()
  end
end

function patterninterpreter(p::Uint)
  mskattpatt::Uint = (MSK_2BITS << (PATTERN_SIZE - PATTERN_GROUP_SIZE)) #mask for attitudinal rule block segmentation

  print("> Pattern ")

  for b::Uint in 1:PATTERN_GROUP_SIZE:PATTERN_SIZE
    i = (sizeof(Int) * 8) - PATTERN_SIZE + b
    j = i + PATTERN_GROUP_SIZE - 1
    print(bits(p)[i:j],(b < PATTERN_SIZE - PATTERN_GROUP_SIZE ? ".": ""))
  end
  println()

  offset::Uint = 0
  for i::Uint in 1:GROUPS
    #Attitudinal aspect
    print(" > \"", attaspect[i], "\" corresponds to ")

    if i < GROUPS
      print("\"", attconcept[((p & mskattpatt) >>> (PATTERN_SIZE - PATTERN_GROUP_SIZE - offset)) + 1], "\"")
    else
      print("\"", performance[((p & mskattpatt) >>> (PATTERN_SIZE - PATTERN_GROUP_SIZE - offset)) + 1], "\"")
    end

    mskattpatt >>= PATTERN_GROUP_SIZE
    offset += PATTERN_GROUP_SIZE

    println()
  end
end

function mutate!(pop::Array{Uint,1}, prob::FloatingPoint)
  for i::Uint = 1:length(pop)
    if(rand() > 1 - prob) #randomly opts to mutation
      pop[i] $= (1 << rand(0:RULE_SIZE - 1)) #mutates one gene (XOR)
    end
  end
end

function fatherbytournament(pop::Array{Uint,1})
  (s1::Uint, s2::Uint, s3::Uint) = rand(1:length(pop), 3) #selects three specimen randomly
  avs1::Uint = objective(loadeddata,pop[s1]) #evaluate specimen #1
  avs2::Uint = objective(loadeddata,pop[s2]) #evaluate specimen #2
  avs3::Uint = objective(loadeddata,pop[s3]) #evaluate specimen #3

  #returns the best of the three specimens
  if(avs1 <= avs2 <= avs3)
    return pop[s1]
  elseif(avs2 <= avs3)
    return pop[s2]
  else
    return pop[s3]
  end
end

function createsnewpopulationbytournament(pop::Array{Uint,1})
  t::Int = length(pop)
  newpop = Array(Uint,t)

  for i::Int in 1:t
    f = fatherbytournament(pop)
    newpop[i] = f
  end

  return newpop #or just 'newpop'
end

function crossing!(pop::Array{Uint,1}, newpop::Array{Uint,1})
  if (t::Int = length(newpop)) != length(pop)
    println("* Populations by (or 'of'???) different sizes. Uncrossed!")
    return
  end

  for i::Int = 1:t

    s1::Uint = newpop[i] #specimen #1
    s2::Uint = pop[i] #specimen #2

    mask::Uint = rand(Uint) & MSK_FULLBITS #generates 18 bits randomly selected
    geness1::Uint = s1 & mask #randomly selects some genes from specimen #1
    geness2::Uint = s2 & mask #randomly selects some genes from specimen #2

    #interchange the selected genes
    mask = ~mask & MSK_FULLBITS
    s1 &= mask
    s2 &= mask
    s1 |= geness2
    s2 |= geness1

    #evaluates the two new specimens
    avs1::Uint = objective(loadeddata,s1) #evaluate specimen #1
    avs2::Uint = objective(loadeddata,s2) #evaluate specimen #2

    #... and keeps the best of both
    if(avs1 > avs2)
      newpop[i] = s1
    else
      newpop[i] = s2
    end

  end

  #place new population on original population
  copy!(pop, newpop)
end

#shows the bits of "n" with the relative position
function debug(n::Uint)
  print(" ")
  for i in 0:31
    print((31-i) % 10)
  end
  println()
  bits(n)
end

#lists the patterns in loaded data
function patterns(d::Dict{Uint,Uint},csv::Bool=false)
  for data in d
    n = data[2]
    if csv
      print(n,",")
    else
      print("> $(lpad(n,3," ")) : ")
    end

    n = bits(data[1])
    for b::Uint in 1:PATTERN_GROUP_SIZE:PATTERN_SIZE
      i = (sizeof(Int) * 8) - PATTERN_SIZE + b
      j = i + PATTERN_GROUP_SIZE - 1
      if csv
        print(n[i:j],",")
      else
        print(n[i:j],(b < PATTERN_SIZE - PATTERN_GROUP_SIZE ? ".": ""))
      end
    end
    println()
  end
end

#the main function
function main(totpop::Int=25, maxit::Int=100,prob::FloatingPoint=0.1,showprimitiverules::Bool=false)

  init()

  tots = load() #loads the patterns data treated and store the total and global total of patterns

#=Replaced by the function signature
  totpop::Int = 15 #total population
  maxit::Uint = 300 #max iterations
  prob::FloatingPoint = 0.1 #probability of mutation
=#

  println("\n> ",rpad("distinct patterns",20,"."),": $(tots[1])")
  println("> ",rpad("total patterns",20,"."),": $(tots[2])")
  println("> ",rpad("population",20,"."),": $(totpop)")
  println("> ",rpad("iterations",20,"."),": $(maxit)")
  println("> ",rpad("probablity",20,"."),": $(prob*100)%")

  pop::Array{Uint,1} = rand(0:MAX_RULE,totpop) #generate the initial population

  i::Int
  for i = 1:maxit
    sel::Array{Uint,1} = createsnewpopulationbytournament(pop)
    crossing!(pop, sel)
    mutate!(pop, prob)
  end

  println()

  if showprimitiverules
    i = 1
    for specie in pop
      #println("= ",lpad(i,2," ")," ",bits(specie)[(sizeof(Int) * 8 - RULE_SIZE) + 1:end])

      #enhanced printing
      for p::Uint in 1:RULE_GROUP_SIZE:RULE_SIZE
        i = (sizeof(Int) * 8) - RULE_SIZE + p
        j = i + RULE_GROUP_SIZE - 1
        print(bits(specie)[i:j],(p < RULE_SIZE - RULE_GROUP_SIZE ? ".": ""))
      end
      println()

      i += 1
    end
  end

  matched::Int = 0
  notmatched::Int = 0
  position::Int = 1
  for specie in pop
    o::Int = objective(loadeddata, specie)
    if o > 0 && (position == 1 || !in(specie,pop[1:position-1]))
      @printf "\n> %d patterns matched [%.1f%%]" o o/tots[2]*100
      ruleinterpreter(specie)
      matched += 1
    else
      notmatched += 1
    end
    position += 1
  end

  print("\n* $(matched) rules matched patterns and ")
  println("$(notmatched) rules matched to none of the $(tots[1]) patterns!")

end













