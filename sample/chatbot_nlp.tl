# Chat Bot with Natural Language Processing
# Demonstrates extension traits, advanced pattern matching, and functional composition

import re
import json
import random

# Define a conversational trait for objects that can respond
export ConversationalTrait = Extension.trait& class(Trait):
    respond_to = Trait.abstract& self => ()
    context = Trait.abstract& self => ()

    # Derived methods available to all implementations
    chat_with = Trait.property& self => user_input =>
        self.respond_to(user_input, self.context)

is_question = text =>
    text.strip().endswith("?") or
        text.lower().startswith(("what", "how", "why", "when", "where", "who", "can", "do", "is", "are"))

# Message processing pipeline using extension methods
Extension.method(str, "extract_entities")& self =>
    # Simple entity extraction
    {
        numbers: re.findall("\\d+", self)
        emails: re.findall("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b", self)
        mentions: re.findall("@(\\w+)", self)
        hashtags: re.findall("#(\\w+)", self)
        urls: re.findall("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\\\(\\\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", self)
    }

Extension.method(str, "sentiment_score")& self =>
    # Simplified sentiment analysis
    let positive_words = ["good", "great", "awesome", "excellent", "fantastic", "love", "like", "happy", "amazing"]
    let negative_words = ["bad", "terrible", "awful", "hate", "dislike", "sad", "angry", "frustrated", "annoying"]

    let words = self.lower().split()
    let positive_count = positive_words.filter(words.__contains__).count()
    let negative_count = negative_words.filter(words.__contains__).count()

    positive_count - negative_count

Extension.method(str, "classify_intent")& self =>
    self.lower() match:
        text if "weather" in text => "weather_query"
        text if ("time" in text or "clock" in text) => "time_query"
        text if ("joke" in text or "funny" in text) => "joke_request"
        text if "help" in text => "help_request"
        text if ("bye" in text or "goodbye" in text) => "farewell"
        text if self.(is_question)() => "question"
        text if self.sentiment_score() > 0 => "positive_statement"
        text if self.sentiment_score() < 0 => "negative_statement"
        _ => "general_statement"

# Weather bot implementation
create_weather_bot = () => {
    context: {
        location: "Unknown"
        last_query: None
        conversation_history: []
    }

    respond_to: Record.method& (self, message, context) =>
        let entities = message.extract_entities()
        let intent = message.classify_intent()
        let sentiment = message.sentiment_score()

        # Update context
        context.conversation_history.append({
            message: message
            intent: intent
            sentiment: sentiment
            timestamp: "now"  # Would use datetime in real implementation
        })

        # Pattern match on intent and entities
        (intent, entities) match:
            ("weather_query", {numbers: [location_code]}) =>
                f"The weather in location {location_code} is sunny with 22°C! 🌞"

            ("weather_query", _) =>
                "I'd love to help with weather! Could you specify a location?"

            ("time_query", _) =>
                "The current time is 3:42 PM! ⏰"

            ("joke_request", _) =>
                random.choice([
                    "Why don't scientists trust atoms? Because they make up everything! 😄"
                    "I told my wife she was drawing her eyebrows too high. She looked surprised! 😂"
                    "Why don't skeletons fight each other? They don't have the guts! 💀"
                ])

            ("help_request", _) =>
                "I can help you with weather, time, jokes, and general conversation! What would you like to know?"

            ("farewell", _) =>
                "Goodbye! It was nice chatting with you! 👋"

            ("question", entities) if entities.numbers =>
                f"That's an interesting question involving the numbers {", ".join(entities.numbers)}!"

            (_, entities) if entities.emails =>
                f"I notice you mentioned an email: {entities.emails[0]}. Are you looking for help with that?"

            (_, _) if sentiment > 1 =>
                "I'm glad you're feeling positive! How can I help make your day even better? 😊"

            (_, _) if sentiment < -1 =>
                "I'm sorry to hear you're not feeling great. Is there anything I can do to help? 🤗"

            _ =>
                "That's interesting! Tell me more about that."
}

# News bot with different personality
create_news_bot = () => {
    context: {
        topics_of_interest: ["technology", "science", "politics"]
        reading_level: "advanced"
        last_articles: []
    }

    respond_to: Record.method& (self, message, context) =>
        let intent = message.classify_intent()
        let entities = message.extract_entities()

        intent match:
            "question" if "news" in message.lower() =>
                "Here are today's top stories: Tech stocks surge, new scientific discovery announced! 📰"

            "question" if "technology" in message.lower() =>
                "Latest in tech: AI advances continue, new programming languages emerge! 💻"

            _ if entities.hashtags =>
                f"I see you're interested in #{entities.hashtags[0]}. Let me find relevant news!"

            _ =>
                "I specialize in news and current events. What topics interest you most?"
}

# Conversation manager using function composition
create_conversation_manager = bots => {
    active_bot: None
    bots: bots

    route_message: Record.method& (self, message) =>
        # Smart routing based on message content
        message.lower() match:
            text if "weather" in text =>
                self.active_bot = self.bots.weather

            text if ("news" in text or "article" in text) =>
                self.active_bot = self.bots.news

            text if ("switch" in text or "change" in text) =>
                # Let user switch bots
                let available = self.bots.keys().list()
                f"Available bots: {", ".join(available)}. Which would you like?"

            _ if self.active_bot === None =>
                # Default to weather bot
                self.active_bot = self.bots.weather

        self.active_bot?.chat_with(message) ?? "I'm not sure how to help with that."
}

# Conversation flow with monadic error handling
run_chat_session = () =>
    print("🤖 Multi-Bot Chat System Starting...")
    print("Available commands: 'quit' to exit, 'switch' to change bots")
    print("Try asking about weather, news, time, or jokes!\n")

    let bots = {
        weather: create_weather_bot()
        news: create_news_bot()
    }

    let manager = create_conversation_manager(bots)

    # Chat loop using recursive function with pattern matching
    let chat_loop = () =>
        let user_input = input("You: ").strip()

        user_input.lower() match:
            "quit" | "exit" | "bye" =>
                print("🤖: Goodbye! Thanks for chatting! 👋")

            "" =>
                print("🤖: I'm here when you're ready to chat!")
                chat_loop()

            message =>
                try:
                    let response = manager.route_message(message)
                    print(f"🤖: {response}\n")
                    chat_loop()
                except KeyboardInterrupt():
                    print("\n🤖: Chat interrupted. Goodbye! 👋")
                except Exception(e):
                    print(f"🤖: Sorry, I encountered an error: {e}")
                    chat_loop()

    chat_loop()

# Demo conversation examples
demo_conversations = () =>
    print("🎭 Running Demo Conversations...\n")

    let weather_bot = create_weather_bot()
    let news_bot = create_news_bot()

    let demo_messages = [
        "What's the weather like in 90210?"
        "Tell me a joke!"
        "What time is it?"
        "Can you help me?"
        "What's the latest news about technology?"
        "I'm feeling great today! #sunshine"
        "Contact me at user@example.com"
        "Goodbye!"
    ]

    demo_messages.for_each(message =>
        print(f"User: {message}")
        print(f"Weather Bot: {weather_bot.chat_with(message)}")
        print(f"News Bot: {news_bot.chat_with(message)}")
        print()
    )

# Main execution
if __name__ == "__main__":
    # Uncomment to run interactive chat:
    # run_chat_session()

    # Run demo instead:
    demo_conversations()
