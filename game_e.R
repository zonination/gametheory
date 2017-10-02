##########
# Game E #
##########

# Separate game file into different responses
game.e<-subset(game, !is.na(choice.e) & user!="zonination")[,c(3, 9)]

# Perform a check for duplicate entries
# game.e[duplicated(game.e$user)|duplicated(game.e$user, fromLast=T),]

# Check to see if there are even or odd columns.
# I'll enter to complete the pairing if there is odd number.
if(nrow(game.e) %% 2 != 0){
  game.e<-rbind(game.e, subset(game, !is.na(choice.e) & user=="zonination")[,c(3, 9)])}

# Begin by randomizing the sample set, and matching opponent.
game.e<-sample_n(game.e, nrow(game.e))
a<-game.e[1:(nrow(game.e)/2),]
b<-game.e[(nrow(game.e)/2 + 1):nrow(game.e),]
game.e<-cbind(a,b); rm(a); rm(b)
names(game.e)<-c("a.user", "a.choice",
                 "b.user", "b.choice")

# What is our end result?
game.e$end<-NA # Clear column
# Map out one of our four scenarios
for(n in 1:nrow(game.e)){
  # Both choose safe
  if(game.e$a.choice[n] == "Peace" &
     game.e$b.choice[n] == "Peace"){
    game.e$end[n] <- paste("Both /u/", game.e$a.user[n]," and /u/", game.e$b.user[n], " live together in an uneasy peace.", sep="")}
  
  # A chooses risky and B chooses safe
  if(game.e$a.choice[n] == "Nuke" &
     game.e$b.choice[n] == "Peace"){
    game.e$end[n] <- paste("/u/", game.e$a.user[n]," strikes first, but with /u/", game.e$b.user[n], "\'s Second Strike protocol, both get annihilated.", sep="")}
  
  # A chooses safe and B chooses risky
  if(game.e$a.choice[n] == "Peace" &
     game.e$b.choice[n] == "Nuke"){
    game.e$end[n] <- paste("/u/", game.e$b.user[n]," strikes first, but with /u/", game.e$a.user[n], "\'s Second Strike protocol, both get annihilated.", sep="")}
  
  # Both choose risky
  if(game.e$a.choice[n] == "Nuke" &
     game.e$b.choice[n] == "Nuke"){
    game.e$end[n] <- paste("Both /u/", game.e$a.user[n]," and /u/", game.e$b.user[n], " strike, and both get annihilated.", sep="")}
}; rm(n)

# Save the file; easy formatting for reddit tables
write_delim(game.e, "game_e.txt", delim="|")
write_csv(game.e, "game_e.csv")

# Plot and save the results
df<-data.frame("y"=c("Nuke","Peace"), "x"=c("Peace","Nuke"))
ggplot(game.e, aes(x=a.choice, y=b.choice))+
  geom_path(data=df, aes(x,y, group=1), lty=2)+
  geom_point(stat="bin2d", aes(size=..count..), shape=21, color="black", fill="firebrick1")+
  geom_text(stat="bin2d", aes(label=..count..))+
  scale_size(range=c(0,25), limits=c(0,120))+
  guides(size=F)+
  labs(title="Mutually Assured Destruction",
       subtitle="Result matrix of scenario",
       x="Subject A",y="Subject B",
       caption="created by /u/zonination")+
  z_theme()
ggsave("game_e.png", height=4, width=5, dpi=120, type="cairo-png")