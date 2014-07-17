function load()

  max::Uint = 4096 #12 bits

  rawdata = readcsv("attitudinal_extracted_treated_data.csv", Uint) #cd("YOUR_FILE_DIRECTORY")!
  patterns = Array(Uint,max)
  fill!(patterns,0)

  #calculates the range of each of different patterns
  for i::Uint in rawdata
    patterns[i+1] += 1 #([i+1]) 1-based for arrays and also acts as differentiator for nulls patterns
  end

  #compute the total of patterns
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

  #returns the total of different patterns and the global total of patterns
  return (total,globaltotal)
end

#get(loadeddata,3,-1) # retorna um valor com base em uma chave de busca

#=
Mining and evolution functions
=#

function init()
  global const BITS1 = 1
  global const BITS2 = 3
  global const PATTERN_SIZE = 12
  global const GROUP_SIZE = 3
  global const GROUPS = 6
  global const RULE_SIZE = GROUP_SIZE * GROUPS
  global const FULLBITS = (~0 >>> (sizeof(Int) * 8 - RULE_SIZE))

  global loadeddata = Dict{Uint,Uint}()

  global const attaspect = [ "Interesse e compromisso", "Assiduidade e pontualidade", "Iniciativa", "Respeito às normas", "Cordialidade", "Desempenho final"]
  global const attconcept = [ "Regular ou Insuficiente", "Bom", "Muito bom", "Ótimo" ]
  global const performance = [ "D", "C", "B", "A" ]
end

#chromosome validation
function objective(d::Dict{Uint,Uint}, r::Uint) #d:data patterns to be analized, r:rule to be applied

  reach::Uint = 0 #objective's reach

  for t in d #t:tuple with (key:pattern in the data group, value:amount of data represented)

    match::Bool = true #rule matches pattern (or not!)
    mskattpatt::Uint = BITS2 << 10 #mask for attitudinal pattern block segmentation
    mskattrule::Uint = BITS2 << 15 #mask for rule block segmentation
    mskpersp::Uint = BITS1 << 17 #mask for perspective operator (0 = less than, 1 = greater or iquals to)

    offsetpatt::Uint = 2 #offset for pattern
    offsetrule::Uint = GROUP_SIZE #offset for rule

    for b::Uint in 1:GROUPS

      #"less than" perspective
      if ((r & mskpersp) >> (RULE_SIZE - offsetrule + 2)) == 0
        #line #138
        if !( ((t[1] & mskattpatt) >> (PATTERN_SIZE - offsetpatt)) < ((r & mskattrule) >> (RULE_SIZE - offsetrule)) )
          match = false
          break
        end

      #"greater or equal than" perspective
      else
        #experiments with '>' or '>='
        #line #140
        if !( ((t[1] & mskattpatt) >> (PATTERN_SIZE - offsetpatt)) >= ((r & mskattrule) >> (RULE_SIZE - offsetrule)) )
          match = false
          break
        end

      end

      #next group
      mskattpatt >>= 2
      mskattrule >>= 3
      mskpersp >>= 3

      offsetrule += 3
      offsetpatt += 2

    end

    if match
      #println(" = $(t[1])")
      reach += t[2]
    end

  end

  return reach
end

function ruleinterpreter(r::Uint)
  #mask for attitudinal rule block segmentation
  mskattrule::Uint = 3 << 15
  #mask for attitudinal perspective operator (0 = less than, 1 = greater or iquals to)
  mskattpersp::Uint = 1 << 17

  print("\n> Rule ")

  for p::Uint in 0:3:15
   i=15+p
   j=17+p
   print(bits(r)[i:j],(p < 15 ? ".": ""))
  end
  println()

  offset::Uint = 0
  for i::Uint in 1:GROUPS
    #Attitudinal aspect
    print(" - ", rpad(attaspect[i],30,"."), " ")

    #Attitudinal perspective
    if (r & mskattpersp) == 0
      print("<   ") #print("less than ")
    else
      print(">=  ") #print("greater than or equals to ")
    end

    if i::Uint < GROUPS
      print("\"", attconcept[((r & mskattrule) >> (15 - offset)) + 1], "\"")
    else
      print("\"", performance[((r & mskattrule) >> (15 - offset)) + 1], "\"")
    end

    mskattrule >>= 3
    mskattpersp >>= 3
    offset = offset + 3

    println()
  end
end

function patterninterpreter(p::Uint)
  mskattpatt::Uint = 3 << 10 #mask for attitudinal rule block segmentation

  println("> Pattern")

  offset::Uint = 0
  for i::Uint in 1:GROUPS
    #Attitudinal aspect
    print(" > ", attaspect[i], " corresponds to ")

    if i < GROUPS
      print("\"", attconcept[((p & mskattpatt) >> (10 - offset)) + 1], "\"")
    else
      print("\"", performance[((p & mskattpatt) >> (10 - offset)) + 1], "\"")
    end

    mskattpatt >>= 2
    offset += 2

    println()
  end
end

function mutate!(pop::Array{Uint,1}, prob::Float32)
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

    mask::Uint = rand(Uint) & FULLBITS #generates 18 bits randomly selected
    geness1::Uint = s1 & mask #randomly selects some genes from specimen #1
    geness2::Uint = s2 & mask #randomly selects some genes from specimen #2

    #interchange the selected genes
    mask = ~mask & FULLBITS
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

function main()

  init()

  tots = load() #loads the patterns data treated and store the total of global patterns
  totpop::Int = 15 #total population
  maxit::Uint = 300 #max iterations
  prob::Float32 = 0.1 #probability of mutation

  println("> ",rpad("distinct patterns",20,"."),": $(tots[1])")
  println("> ",rpad("total patterns",20,"."),": $(tots[2])")
  println("> ",rpad("population",20,"."),": $(totpop)")
  println("> ",rpad("iterations",20,"."),": $(maxit)")
  println("> ",rpad("probablity",20,"."),": $(prob*100)%\n")

  pop::Array{Uint,1} = rand(0:FULLBITS,totpop) #generate the initial population

  i::Int
  for i = 1:maxit
    sel::Array{Uint,1} = createsnewpopulationbytournament(pop)
    crossing!(pop, sel)
    mutate!(pop, prob)
  end

  println()

  i = 1
  for specie in pop
    println("= ",lpad(i,2," ")," ",bits(specie)[15:end])
    i += 1
  end

  count::Int = 0
  for specie in pop
    o::Int = objective(loadeddata, specie)
    if o > 0
      @printf "\n> %d patterns matched [%.1f%%]" o o/tots[2]*100
      ruleinterpreter(specie)
    else
      count += 1
    end
  end

  if count != 0
    println("\n* $(count) rules matched to none of the $(tots[1]) patterns!")
  end

end













