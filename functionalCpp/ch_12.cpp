#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>

// Ch 12 Funcitonal design for concurrent systems
// Instead of having immutable data that is shared,
// we can have mutable data that is never shared

// We don't need to modify other objects, we can
// send them messages and let them react to those
// messages

// Actor model: actors are isolated entities
// that can't share anything but can send messages
// to each other. They should know who they're
// sending to and have multiple message types

// In a simplified version, we can define a single
// message type and let an externel controller
// deal with the message routing

// Receives SourceMessage, sends Message
template <typename SourceMessageType,
          typename MessageType>
class Actor
{
  public:
    // Make the type accessible
    using value_type = MessageType;
    void process_message(SourceMessageType&& message);

    // Sets the handler the actor calls when it wants
    // to send a message
    template <typename EmitFunction>
    void set_message_handler(EmitFunction emit); 

  private:
    std::function<void(MessageType&&)> m_emit;
};

// Sinks: actors that just receive messages
// Sources: actors that just send messages
// General actors: actors that do both


int main(int argc, char** argv)
{
  return 0;
}
