##########
# Game D #
##########

# Separate game file into different responses
game.d<-subset(game, !is.na(choice.d) & user!="zonination")[,c(1,3, 8)]

# Perform a check for duplicate entries
# game.d[duplicated(game.d$user)|duplicated(game.d$user, fromLast=T),]

# Check to see if there are even or odd columns.
# I'll enter to complete the pairing if there is odd number.
if(nrow(game.d) %% 2 != 0){
  game.d<-rbind(game.d, subset(game, !is.na(choice.d) & user=="zonination")[,c(1,3, 8)])}

# Begin by randomizing the sample set, and matching opponent.
game.d<-sample_n(game.d, nrow(game.d))
a<-game.d[1:(nrow(game.d)/2),]
b<-game.d[(nrow(game.d)/2 + 1):nrow(game.d),]
game.d<-cbind(a,b); rm(a); rm(b)
names(game.d)<-c("a.time", "a.user", "a.choice",
                 "b.time", "b.user", "b.choice")

# What is our end result?
game.d$end<-NA # Clear column
# Map out one of our four scenarios
for(n in 1:nrow(game.d)){
  # Both choose safe
  if(game.d$a.choice[n] == "Peace" &
     game.d$b.choice[n] == "Peace"){
    game.d$end[n] <- paste("Both /u/", game.d$a.user[n]," and /u/", game.d$b.user[n], " are peaceful.", sep="")}
  
  # A chooses risky and B chooses safe
  if(game.d$a.choice[n] == "Nuke" &
     game.d$b.choice[n] == "Peace"){
    game.d$end[n] <- paste("/u/", game.d$a.user[n]," nukes and destroys /u/", game.d$b.user[n], " out of opportunity.", sep="")}
  
  # A chooses safe and B chooses risky
  if(game.d$a.choice[n] == "Peace" &
     game.d$b.choice[n] == "Nuke"){
    game.d$end[n] <- paste("/u/", game.d$b.user[n]," nukes and destroys /u/", game.d$a.user[n], " out of opportunity.", sep="")}
  
  # Both choose risky
  if(game.d$a.choice[n] == "Nuke" &
     game.d$b.choice[n] == "Nuke"){
    #Figure out who strikes first
    if(game.d$a.time[n]<game.d$b.time[n]){
      game.d$end[n] <- paste("Both try to strike first, but /u/", game.d$a.user[n]," was quicker and annihilates /u/", game.d$b.user[n], " first.", sep="")}
    if(game.d$a.time[n]>=game.d$b.time[n]){
      game.d$end[n] <- paste("Both try to strike first, but /u/", game.d$b.user[n]," was quicker and annihilates /u/", game.d$a.user[n], " first.", sep="")}}
}; rm(n)

# Save the file; easy formatting for reddit tables
write_delim(game.d, "game_d.txt", delim="|")
write_csv(game.d, "game_d.csv")

# Plot and save the results
df<-data.frame("y"=c("Nuke","Peace"), "x"=c("Peace","Nuke"))
ggplot(game.d, aes(x=a.choice, y=b.choice))+
  geom_path(data=df, aes(x,y, group=1), lty=2)+
  geom_point(stat="bin2d", aes(size=..count..), shape=21, color="black", fill="gold")+
  geom_text(stat="bin2d", aes(label=..count..))+
  scale_size(range=c(0,25), limits=c(0,120))+
  guides(size=F)+
  labs(title="First Strike",
       subtitle="Result matrix of scenario",
       x="Subject A",y="Subject B",
       caption="created by /u/zonination")+
  z_theme()
ggsave("game_d.png", height=4, width=5, dpi=120, type="cairo-png")