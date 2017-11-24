/**
 * Contains the list of possible exceptions the Client can trigger and the error codes and messages
 * associated with those exceptions
 */
public enum ClientExceptionTypes {
    INVALID_SYNTAX(0, "Invalid syntax"),
    UNKNOWN_COMMAND(1, "Unknown command"),
    UNKNOWN_SUB_COMMAND(2, "Unknown sub command"),
    NO_CHATROOM_FOUND(3, "No chat room found"),
    NOT_IN_CHATROOM(4, "You may not send messages to chat rooms you haven't yet joined.");

    private int errorCode;
    private String errorMessage;


    ClientExceptionTypes(int errorCode, String errorMessage) {
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public String getErrorMessage(){
        return errorMessage;
    }

    public int getErrorCode() {
        return errorCode;
    }

}