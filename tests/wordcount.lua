
file, err = io.open("historia de la computacion.txt", "r")
if (file == nil) then
  print(err)
  os.exit()
end
text = file:read("*all")

text_pos = 1

function validchar (char)
  if char == nil or char == "" then return false end
  byte = string.byte(char)
  min = string.byte("a")
  max = string.byte("z")
  return char == " " or (byte >= min and byte <= max)
end

function nextchar ()
  char = string.sub(text, text_pos, text_pos)
  text_pos = text_pos + 1
  if char == "" then return nil end
  char = string.lower(char)
  if validchar(char) then
    return char
  else
    return nextchar()
  end
end

function iswordend (char)
  return char == nil or
    char == " " or char == "\n" or
    char == "\r" or char == "\t";
end

function nextword ()
  word = ""
  char = nextchar()
  if char == nil then return nil end
  while iswordend(char) and char ~= nil do char = nextchar() end
  while (not iswordend(char)) do
    word = word .. char
    char = nextchar()
  end
  return word
end

words = {}

function addword (word)
  local count = words[word]
  if (count == nil) then
    words[word] = 1
  else
    words[word] = count + 1
  end
end

word = nextword()
while word ~= nil do
  addword(word)
  word = nextword()
end

list = {}

-- Insertion sort
function insert (nd)
  local i = #list
  while (i >= 1 and nd.count < list[i].count) do
    list[i+1] = list[i]
    i = i-1
  end
  list[i+1] = nd
end

for k,v in pairs(words) do
  nd = {str=k, count=v}
  insert(nd)
end

local i = 1
while (i <= #list) do
  local nd = list[i]
  print(nd.str .. " : " .. nd.count)
  i = i+1
end