import java.io.*;
import java.net.Socket;
import java.util.HashMap;


public class Client implements Runnable {

    /**
     * Defines all of the commands expected by the server and the total number of lines
     * that the command expects.
     */
    enum Command {
        HELO("HELO", 0),
		ANYTHING("ANYTHING", 0),
        KILL_SERVICE("KILL_SERVICE", 0),
        JOIN_CHATROOM("JOIN_CHATROOM", 4),
        LEAVE("LEAVE_CHATROOM", 3),
        DISCONNECT("DISCONNECT", 3),
        CHAT("CHAT", 4);

        private String command;
        private int numSubCommandLines;

        Command(String command, int numSubCommandLines) {
            this.command = command;
            this.numSubCommandLines = numSubCommandLines;
        }
    }

    /**
     * Defines the allowed command parameters
     */
    enum SubCommands {
        CLIENT_NAME,
        JOIN_ID,
        MESSAGE,
        CLIENT_IP,
        PORT
    }


    private boolean running = false;
    private Server server;
    private Socket socket;
    private BufferedReader fromClient;
    private MyBufferedWriter toClient;
    private int clientId;
    private HashMap<Integer, String> clientsNamesInChatrooms;


    /**
     * Handles all interaction between the Client and the Server.
     * @param server the Server instance currently running
     * @param socket the created Socket connection between the Client and Server
     * @param clientId the Id of the Client (specified by Server)
     */
    Client(Server server, Socket socket, int clientId) {
        this.running = true;
        this.server = server;
        this.socket = socket;
        this.clientId = clientId;
        this.clientsNamesInChatrooms = new HashMap<>();

        try {
            fromClient = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            toClient = new MyBufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * Polls the socket for data from the Client
     */
    @Override
    public void run() {
        try {
            while(running) {
                String firstLine = fromClient.readLine();
                if(firstLine != null) {
                    handleCommand(firstLine);
                }

            }

        }

        // Closing the socket connection while readLine() is blocking throws an IOException.
        // This happens when another client issues the "KILL_SERVICE" command and is thus acceptable.
        catch (IOException e) {
            System.out.println(clientId + " interrupted, the server must be shutting down.");
        }
    }



    /*******************************
     Private Instance Methods
     *******************************/


    /**
     * Parses the data sent from the client and runs the appropriate callback method.
     */
    private void handleCommand(String firstLine) {
        try {
            System.out.println("Received:" + firstLine);
            // Extract the command and parameter from the line read from the socket
            String[] parts = getCommandAndParams(firstLine);
            String command = parts[0];
            String params = parts[1];
			
            if (command.equals(Command.HELO.command)) {
                respondToHelo(params);
            } else if(command.equals(Command.JOIN_CHATROOM.command)) {
                joinChatRoom(params);
            } else if (command.equals(Command.CHAT.command)) {
                chat(params);
            } else if (command.equals(Command.LEAVE.command)) {
                leaveChatRoom(params);
            } else if (command.equals(Command.DISCONNECT.command)) {
                disconnect();
            } else if (command.equals(Command.KILL_SERVICE.command)) {
                killService();
		    } else if (command.equals(Command.ANYTHING.command)) {
                killService();
            } else {
                //throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient, firstLine);
				System.out.println("Unknown command!\n");
            }
        } catch (ClientException c) {
            //c.printStackTrace();
            //c.sendError();
        }
    }

    /**
     * Responds to the HELO command
     * @param text the parameter text sent with the HELO command
     */
    private void respondToHelo(String text) {
        String[] messages = {
            "HELO " + text + "\n" +
			"IP:188.141.99.211\n" +
            //"IP:" + server.getIP() + "\n" +
            "Port:" + server.getPort() + "\n" +
            "StudentID:13320571"};

        sendMessageToClient(messages);
    }

    /**
     * Handles the client request to join the ChatRoom with the specified name.
     * @param chatRoomName name of the ChatRoom the Client wishes to join
     */
    private void joinChatRoom(String chatRoomName) {
        try {
            // Throw away CLIENT_IP, PORT Fields
			//DEBUG
			//System.out.println("Entered joinChatRoom func");
            String[] parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.CLIENT_IP);

            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.PORT);

            // Get Clients name
            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.CLIENT_NAME);
            String clientNameInThisChatRoom = parts[1];
            server.joinChatRoom(this, clientNameInThisChatRoom, chatRoomName);
			//DEBUG
            //System.out.println("Sent joinChatRoom func to server");
        } catch (ClientException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Handles the Client request to leave a ChatRoom
     * @param roomId the Id of the ChatRoom the Client wishes to leave.
     */
    private void leaveChatRoom(String roomId) {
        try {
            int roomReferenceNumber = Integer.valueOf(roomId);

            // Throw away joinId (stored as clientId)
            String[] parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.JOIN_ID);

            // Throw away clientName (stored as in instance HashMap)
            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.CLIENT_NAME);

            // Ensure ChatRoom exists
            ChatRoom chatRoom = server.getChatRoomById(roomReferenceNumber);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            }

            chatRoom.leaveChatRoom(this);

        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Handles the Client request to disconnect from the server
     */
    private void disconnect() {
        try {
            // Throw away Port
            String[] parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.PORT);

            // Throw away Client's name
            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.CLIENT_NAME);

            // Disconnect client
            server.terminateClientConnection(this);
            safelyTerminate();

        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }

    }

    /**
     *  Handles a Client request send a message to a ChatRoom
    */
    private void chat(String roomRef) {
        try {
            int chatRoomId = Integer.valueOf(roomRef);
			System.out.println("beggining chat fuction");
            // Ensure ChatRoom exists and Client is in it
            ChatRoom chatRoom = server.getChatRoomById(chatRoomId);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            } else if(!chatRoom.isClientInChatRoom(this.clientId)) {
                throw new ClientException(ClientExceptionTypes.NOT_IN_CHATROOM, toClient);
            }

            // Throw away join id
            String[] parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.JOIN_ID);

            // Throw away client name
            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.CLIENT_NAME);


            // TODO: Maybe add multi line message functionality
            parts = getCommandAndParams(fromClient.readLine());
            validateSubCommand(parts[0], SubCommands.MESSAGE);
            String message = parts[1];

            // Read second newline character that terminates the Message
            fromClient.readLine();
			System.out.println("Our message:" + message);
            chatRoom.chat(this, message);

        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }


    }

    /**
     * Handles a Client request to kill the service (shuts down server)
     */
    private void killService() {
        server.killService();
    }

    /**
     * Extracts the command and parameters from a line read from the Client Socket
     * @param line the line read from the Socket
     * @return an array containing the command and the parameter as Strings
     * @throws ClientException if could not extract command (didn't contain ': ')
     */
    private String[] getCommandAndParams(String line) throws ClientException {
        String[] parts = line.split(": ");

        if(parts.length == 1) {
            // No semi colon found, may be a HELO or KILL_SERVICE command.
            if(parts[0].contains(Command.HELO.command)) {
                String[] parts2 = {Command.HELO.command, parts[0].substring(5)};
                return parts2;
            } else if(parts[0].contains(Command.KILL_SERVICE.command)) {
                String[] parts2 = {Command.KILL_SERVICE.command, null};
                return parts2;
            }
            else {
                throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient, line);
            }
        }

        parts[0] = parts[0].toUpperCase();
        return parts;
    }

    /**
     * Checks that a sub command (command that came after main command) is a valid
     * @param command the sub command read.
     * @param expected one of the defined accepted SubCommands
     * @throws ClientException if sub command did not matched the expected SubCommand
     */
    private void validateSubCommand(String command, Enum<SubCommands> expected) throws ClientException{
        if(!command.equals(expected.name())) {
            System.out.println("Throwing error");
            throw new ClientException(ClientExceptionTypes.UNKNOWN_SUB_COMMAND, toClient);
        }
    }



    /*******************************
        API Methods
     *******************************/

    /**
     * Sends an array of messages (each terminated with a newline) to the client immediately.
     * @param messages the lines to be sent to the client
     */
    public void sendMessageToClient(String[] messages) {
        try {
            for(String message : messages) 
			{
                toClient.writeLine(message);
				//System.out.println(message);
				
            }
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void sendMessageToClientJoin(String[] messages) {
        try {
            for(String message : messages) 
			{
                toClient.writeLineJoin(message);
				//System.out.println(message);
				
            }
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    /**
     * Safely terminates the client's connection to the server.
     */
    public void safelyTerminate() {
        this.running = false;
        System.out.println("Shutting down " + this.clientId);
        try {
            socket.close();
            toClient.close();
            fromClient.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }





    /*******************************
        Getters and Setters
     *******************************/

    /**
     * @return the clientId of the Client instance
     */
    public int getClientId() {
        return this.clientId;
    }
    
    /**
     * @param chatRoomId the Id of the ChatRoom instance
     * @return the name of this Client in that ChatRoom
     */
    public String getNameInChatroom(int chatRoomId) {
        return this.clientsNamesInChatrooms.get(chatRoomId);
    }

    /**
     * Sets the name of the Client in the ChatRoom instance which has the given Id
     * @param chatRoomId the Id of the ChatRoom
     * @param name the desired name of this Client in the ChatRoom
     */
    public void setNameInChatroom(int chatRoomId, String name) {
        clientsNamesInChatrooms.put(chatRoomId, name);
    }

}
