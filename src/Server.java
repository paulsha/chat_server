import java.io.IOException;
import java.net.*;
import java.util.ArrayList;
import java.util.HashMap;


public class Server {

    private static final int DEFAULT_PORT = 3000;
    private static final String EXTERNAL_IP_API = "https://api.ipify.org?format=json";    // For testing from home
    private static final boolean TESTING_FROM_EXTERNAL_NETWORK = false;

    public static void main(String[] args) {
        int port;
        if(args.length > 0) {
            port = Integer.valueOf(args[0]);
        } else {
            port = DEFAULT_PORT;
        }
        new Server(port);
    }


    private ServerSocket serverSocket;
    private volatile boolean running = false;
    private HashMap<Integer, Client> clients;
    private ArrayList<Thread> threads;
    private HashMap<Integer, ChatRoom> chatRooms;
    private static int nextChatRoomId = 0;
    private static int nextClientJoinId = 0;

    /**
     * Creates the Server
     * @param port the port to run the Server on
     */
    public Server(int port) {
        try {
            serverSocket = new ServerSocket(port);
            clients = new HashMap<>();
            threads = new ArrayList<>();
            chatRooms = new HashMap<>();
            System.out.println("Server started at " + getIP() +  " on port " + getPort());
            createChatRoom("Default");
            running = true;
            listen();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void listen() {
        while(running) {
            try {
                Socket socket = serverSocket.accept();
                Client client = new Client(this, socket, nextClientJoinId);
                System.out.println("Client " + nextClientJoinId + " created");
                Thread thread = new Thread(client);
                thread.start();
                clients.put(nextClientJoinId, client);
				System.out.println("New client added to hastable");
				nextClientJoinId++;

            } catch (SocketException s) {
                System.out.println("Shutting down server");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


    /*******************************
        API Methods
     *******************************/

    /**
     * Inserts a client into a ChatRoom instance allowing them to receive messages from and
     * send messages to the ChatRoom
     * @param client the client instance that wishes to join the ChatRoom
     * @param clientNameInThisChatRoom the desired client handle in that ChatRoom
     * @param name the name of the ChatRoom the client wishes to join
     */
    public void joinChatRoom(Client client, String clientNameInThisChatRoom, String name) {
        for(ChatRoom chatRoom : chatRooms.values()) {
            if(chatRoom.getChatRoomName().equals(name)) {
                chatRoom.joinChatRoom(client, clientNameInThisChatRoom);
				//DEBUG
                //System.out.println("joinChatRoom: Existing chatroom found");
                return;
            }
        }

        // If chatroom does not exist, create one and subscribe to it
        ChatRoom chatRoom =  createChatRoom(name);
        chatRoom.joinChatRoom(client, clientNameInThisChatRoom);
    }

    /**
     * Creates a new ChatRoom instance with the provided chat room name
     * @param name the desired name of the ChatRoom
     * @return the ChatRoom instance
     */
    private ChatRoom createChatRoom(String name) {
        ChatRoom chatRoom = new ChatRoom(this, nextChatRoomId, name);
        chatRooms.put(nextChatRoomId++, chatRoom);
        return chatRoom;
    }

    /**
     * Safely terminates all client connections and shuts down the service
     * In a real application this obviously would not be front facing.
     */
    public void killService() {
        this.running = false;
        for(Client client : clients.values()) {
            client.safelyTerminate();
        }
        for(Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }
        try {
            serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * Called by client when it wants to disconnect. This informs all ChatRoom instances that this client will be
     * disconnecting, allowing them to remove the client from their list of connected clients.
     * @param client the client who has terminated
     */
    public void terminateClientConnection(Client client) {
        for(ChatRoom chatRoom : chatRooms.values()) {
            chatRoom.notifyOfClientTermination(client);
        }

        clients.remove(client.getClientId());
    }



    /*******************************
     Getters and Setters
     *******************************/

    /**
     * Returns ChatRoom instance that has the provided Id
     * @param chatRoomId the Id of the ChatRoom
     * @return ChatRoom instance if ChatRoom exists, null if it does not.
     */
    public ChatRoom getChatRoomById(int chatRoomId) {
        return chatRooms.get(chatRoomId);
    }

    /**
     * Fetches current external IP address from an API if TESTING_FROM_EXTERNAL_NETWORK is set
     * @return the IP address that the Server is running on.
     */
    public String getIP() {
        if(TESTING_FROM_EXTERNAL_NETWORK) {
            try (java.util.Scanner s = new java.util.Scanner(new java.net.URL(EXTERNAL_IP_API).openStream(), "UTF-8").useDelimiter("\\A")) {
                String str = s.next().split("\":\"")[1];     // extracts xx.xx.xx.xx" from JSON
                return str.substring(0, str.length() - 2);
            } catch (IOException e) {
                e.printStackTrace();
                return "UNKNOWN IP ADDRESS";
            }
        } else {
            try {
                return InetAddress.getLocalHost().getHostAddress();
            } catch (IOException e) {
                e.printStackTrace();
                return "UNKNOWN IP ADDRESS";
            }
        }
    }

    /**
     * @return the port the server is currently running on
     */
    public int getPort() {
        return this.serverSocket.getLocalPort();
    }
}

